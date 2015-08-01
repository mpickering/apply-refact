{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Refact.Fixity (applyFixities) where

import SrcLoc

import Refact.Utils
import BasicTypes (Fixity(..), defaultFixity, compareFixity, negateFixity, FixityDirection(..))
import HsExpr
import RdrName
import OccName
import PlaceHolder
import Data.Generics hiding (Fixity)
import Data.Maybe
import Language.Haskell.GHC.ExactPrint.Types

import Control.Monad.State
import qualified Data.Map as Map
import Data.Tuple

-- | Rearrange infix expressions to account for fixity.
-- The set of fixities is wired in and includes all fixities in base.
applyFixities :: Anns -> Module -> (Anns, Module)
applyFixities as m = swap $ runState (everywhereM (mkM expFix) m) as

expFix :: LHsExpr RdrName -> M (LHsExpr RdrName)
expFix (L loc (OpApp l op _ r)) = do
  newExpr <- mkOpAppRn baseFixities l op (findFixity baseFixities op) r
  return (L loc newExpr)

expFix e = return e

getIdent :: Expr -> String
getIdent (unLoc -> HsVar n) = occNameString . rdrNameOcc $ n
getIdent _ = error "Must be HsVar"


moveDelta :: AnnKey -> AnnKey -> M ()
moveDelta old new = do
  a@Ann{..} <- gets (fromMaybe annNone . Map.lookup old)
  modify (Map.insert new (annNone { annEntryDelta = annEntryDelta, annPriorComments = annPriorComments }))
  modify (Map.insert old (a { annEntryDelta = DP (0,0), annPriorComments = []}))

---------------------------
-- Modified from GHC Renamer
mkOpAppRn ::
             [(String, Fixity)]
          -> LHsExpr RdrName              -- Left operand; already rearrange
          -> LHsExpr RdrName -> Fixity            -- Operator and fixity
          -> LHsExpr RdrName                      -- Right operand (not an OpApp, but might
                                                -- be a NegApp)
          -> M (HsExpr RdrName)

-- (e11 `op1` e12) `op2` e2
mkOpAppRn fs e1@(L _ (OpApp e11 op1 p e12)) op2 fix2 e2
  | nofix_error
  = return $ OpApp e1 op2 p e2

  | associate_right = do
    new_e <- L loc' <$> mkOpAppRn fs e12 op2 fix2 e2
    moveDelta (mkAnnKey e12) (mkAnnKey new_e)
    return $ OpApp e11 op1 p new_e
  where
    fix1 = findFixity fs op1
    loc'= combineLocs e12 e2
    (nofix_error, associate_right) = compareFixity fix1 fix2

---------------------------
--      (- neg_arg) `op` e2
mkOpAppRn fs e1@(L _ (NegApp neg_arg neg_name)) op2 fix2 e2
  | nofix_error
  = return $ OpApp e1 op2 PlaceHolder e2

  | associate_right
  = do
      new_e <- L loc' <$> mkOpAppRn fs neg_arg op2 fix2 e2
      moveDelta (mkAnnKey neg_arg) (mkAnnKey new_e)
      return (NegApp new_e neg_name)
  where
    loc' = combineLocs neg_arg e2
    (nofix_error, associate_right) = compareFixity negateFixity fix2

---------------------------
--      e1 `op` - neg_arg
mkOpAppRn _ e1 op1 fix1 e2@(L _ (NegApp _ _))     -- NegApp can occur on the right
  | not associate_right                 -- We *want* right association
  = return $ OpApp e1 op1 PlaceHolder e2
  where
    (_, associate_right) = compareFixity fix1 negateFixity

---------------------------
--      Default case
mkOpAppRn _ e1 op _ e2                  -- Default case, no rearrangment
  = return $ OpApp e1 op PlaceHolder e2

findFixity :: [(String, Fixity)] -> Expr -> Fixity
findFixity fs r = askFix fs (getIdent r)

askFix :: [(String, Fixity)] -> String -> Fixity
askFix xs = \k -> lookupWithDefault defaultFixity k xs
    where
        lookupWithDefault def k mp1 = fromMaybe def $ lookup k mp1



-- | All fixities defined in the Prelude.
preludeFixities :: [(String, Fixity)]
preludeFixities = concat
    [infixr_ 9  ["."]
    ,infixl_ 9  ["!!"]
    ,infixr_ 8  ["^","^^","**"]
    ,infixl_ 7  ["*","/","quot","rem","div","mod",":%","%"]
    ,infixl_ 6  ["+","-"]
    ,infixr_ 5  [":","++"]
    ,infix_  4  ["==","/=","<","<=",">=",">","elem","notElem"]
    ,infixr_ 3  ["&&"]
    ,infixr_ 2  ["||"]
    ,infixl_ 1  [">>",">>="]
    ,infixr_ 1  ["=<<"]
    ,infixr_ 0  ["$","$!","seq"]
    ]

-- | All fixities defined in the base package.
--
--   Note that the @+++@ operator appears in both Control.Arrows and
--   Text.ParserCombinators.ReadP. The listed precedence for @+++@ in
--   this list is that of Control.Arrows.
baseFixities :: [(String, Fixity)]
baseFixities = preludeFixities ++ concat
    [infixl_ 9 ["!","//","!:"]
    ,infixl_ 8 ["shift","rotate","shiftL","shiftR","rotateL","rotateR"]
    ,infixl_ 7 [".&."]
    ,infixl_ 6 ["xor"]
    ,infix_  6 [":+"]
    ,infixl_ 5 [".|."]
    ,infixr_ 5 ["+:+","<++","<+>"] -- fixity conflict for +++ between ReadP and Arrow
    ,infix_  5 ["\\\\"]
    ,infixl_ 4 ["<$>","<$","<*>","<*","*>","<**>"]
    ,infix_  4 ["elemP","notElemP"]
    ,infixl_ 3 ["<|>"]
    ,infixr_ 3 ["&&&","***"]
    ,infixr_ 2 ["+++","|||"]
    ,infixr_ 1 ["<=<",">=>",">>>","<<<","^<<","<<^","^>>",">>^"]
    ,infixl_ 0 ["on"]
    ,infixr_ 0 ["par","pseq"]
    ]

infixr_, infixl_, infix_ :: Int -> [String] -> [(String,Fixity)]
infixr_ = fixity InfixR
infixl_ = fixity InfixL
infix_  = fixity InfixN

-- Internal: help function for the above definitions.
fixity :: FixityDirection -> Int -> [String] -> [(String, Fixity)]
fixity a p = map (,Fixity p a)

