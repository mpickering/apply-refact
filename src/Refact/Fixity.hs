{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Refact.Fixity (doFix) where

import SrcLoc

import Refact.Utils
import BasicTypes (Fixity(..), defaultFixity, compareFixity, negateFixity, FixityDirection(..))
import HsExpr
import RdrName
import OccName
import Name
import PlaceHolder
import Data.Generics hiding (Fixity)
import Data.Maybe
import Language.Haskell.GHC.ExactPrint.Utils

import Control.Monad.Identity

-- Useful for debugging
--deriving instance Show Fixity
--deriving instance Show FixityDirection

-- | Rearrange infix expressions to account for fixity
-- Note that this does not require changes to the annotations as OpApp has no annotations
doFix :: Module -> Module
doFix = everywhere (mkT expFix)

expFix :: LHsExpr RdrName -> LHsExpr RdrName
expFix (L loc (OpApp l op p r)) =
  L loc (mkOpAppRn baseFixities l op (findFixity baseFixities op) r)
expFix e = e

getIdent (unLoc -> HsVar n) = occNameString . rdrNameOcc $ n
getIdent _ = error "Must be HsVar"


---------------------------
-- Modified from GHC Renamer
mkOpAppRn ::
             [(String, Fixity)]
          -> LHsExpr RdrName              -- Left operand; already rearrange
          -> LHsExpr RdrName -> Fixity            -- Operator and fixity
          -> LHsExpr RdrName                      -- Right operand (not an OpApp, but might
                                                -- be a NegApp)
          -> HsExpr RdrName

-- (e11 `op1` e12) `op2` e2
mkOpAppRn fs e1@(L _ (OpApp e11 op1 p e12)) op2 fix2 e2
  | nofix_error
  = OpApp e1 op2 p e2

  | associate_right =
    let new_e = mkOpAppRn fs e12 op2 fix2 e2
    in OpApp e11 op1 p (L loc' new_e)
  where
    fix1 = findFixity fs op1
    loc'= combineLocs e12 e2
    (nofix_error, associate_right) = compareFixity fix1 fix2

---------------------------
--      (- neg_arg) `op` e2
mkOpAppRn fs e1@(L _ (NegApp neg_arg neg_name)) op2 fix2 e2
  | nofix_error
  = OpApp e1 op2 PlaceHolder e2

  | associate_right
  =
      let new_e = mkOpAppRn fs neg_arg op2 fix2 e2
      in (NegApp (L loc' new_e) neg_name)
  where
    loc' = combineLocs neg_arg e2
    (nofix_error, associate_right) = compareFixity negateFixity fix2

---------------------------
--      e1 `op` - neg_arg
mkOpAppRn fs e1 op1 fix1 e2@(L _ (NegApp _ _))     -- NegApp can occur on the right
  | not associate_right                 -- We *want* right association
  = OpApp e1 op1 PlaceHolder e2
  where
    (_, associate_right) = compareFixity fix1 negateFixity

---------------------------
--      Default case
mkOpAppRn fs e1 op fix e2                  -- Default case, no rearrangment
  = OpApp e1 op PlaceHolder e2

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

