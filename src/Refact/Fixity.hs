{-# LANGUAGE ViewPatterns #-}

module Refact.Fixity (applyFixities) where

import Control.Monad.Trans.State
import Data.Generics hiding (Fixity)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Tuple
import qualified GHC
import Language.Haskell.GHC.ExactPrint.Types hiding (GhcPs, GhcRn, GhcTc)
import Refact.Compat (Fixity (..), SourceText (..), occNameString, rdrNameOcc)
import Refact.Utils

-- | Rearrange infix expressions to account for fixity.
-- The set of fixities is wired in and includes all fixities in base.
applyFixities :: Anns -> Module -> IO (Anns, Module)
applyFixities as m = swap <$> runStateT (everywhereM (mkM expFix) m) as

expFix :: Expr -> StateT Anns IO Expr
expFix (GHC.L loc (GHC.OpApp _ l op r)) =
  mkOpAppRn baseFixities loc l op (findFixity baseFixities op) r
expFix e = return e

getIdent :: Expr -> String
getIdent (GHC.unLoc -> GHC.HsVar _ (GHC.L _ n)) = occNameString . rdrNameOcc $ n
getIdent _ = error "Must be HsVar"

-- | Move the delta position from one annotation to another:
--
--  * When rewriting '(e11 `op1` e12) `op2` e2' into 'e11 `op1` (e12 `op2` e2)', move the delta position
--    from 'e12' to '(e12 `op2` e2)'.
--  * When rewriting '(- neg_arg) `op` e2' into '- (neg_arg `op` e2)', move the delta position
--    from 'neg_arg' to '(neg_arg `op` e2)'.
moveDelta :: Annotation -> AnnKey -> AnnKey -> StateT Anns IO ()
moveDelta oldAnn oldKey newKey = do
  -- If the old annotation has a unary minus operator, add it to the new annotation.
  let newAnnsDP
        | Just dp <- find ((== G GHC.AnnMinus) . fst) (annsDP oldAnn) = [dp]
        | otherwise = []
  modify . Map.insert newKey $
    annNone
      { annEntryDelta = annEntryDelta oldAnn,
        annPriorComments = annPriorComments oldAnn,
        annsDP = newAnnsDP
      }

  -- If the old key is still there, reset the value.
  modify $ Map.adjust (\a -> a {annEntryDelta = DP (0, 0), annPriorComments = []}) oldKey

---------------------------
-- Modified from GHC Renamer
mkOpAppRn ::
  [(String, GHC.Fixity)] ->
  GHC.SrcSpan ->
  Expr -> -- Left operand; already rearrange
  Expr ->
  GHC.Fixity -> -- Operator and fixity
  Expr -> -- Right operand (not an OpApp, but might
  -- be a NegApp)
  StateT Anns IO Expr
-- (e11 `op1` e12) `op2` e2
mkOpAppRn fs loc e1@(GHC.L _ (GHC.OpApp x1 e11 op1 e12)) op2 fix2 e2
  | nofix_error =
    return $ GHC.L loc (GHC.OpApp noExt e1 op2 e2)
  | associate_right = do
    let oldKey = mkAnnKey e12
    oldAnn <- gets $ Map.findWithDefault annNone oldKey
    new_e <- mkOpAppRn fs loc' e12 op2 fix2 e2
    let newKey = mkAnnKey new_e
    moveDelta oldAnn oldKey newKey
    return $ GHC.L loc (GHC.OpApp x1 e11 op1 new_e)
  where
    loc' = GHC.combineLocs e12 e2
    fix1 = findFixity fs op1
    (nofix_error, associate_right) = GHC.compareFixity fix1 fix2

---------------------------
--      (- neg_arg) `op` e2
mkOpAppRn fs loc e1@(GHC.L _ (GHC.NegApp _ neg_arg neg_name)) op2 fix2 e2
  | nofix_error =
    return (GHC.L loc (GHC.OpApp noExt e1 op2 e2))
  | associate_right =
    do
      let oldKey = mkAnnKey neg_arg
      oldAnn <- gets $ Map.findWithDefault annNone oldKey
      new_e <- mkOpAppRn fs loc' neg_arg op2 fix2 e2
      let newKey = mkAnnKey new_e
      moveDelta oldAnn oldKey newKey
      let res = GHC.L loc (GHC.NegApp noExt new_e neg_name)
          key = mkAnnKey res
          ak = AnnKey (srcSpanToAnnSpan loc) (CN "OpApp")
      opAnn <- gets (fromMaybe annNone . Map.lookup ak)
      negAnns <- gets (fromMaybe annNone . Map.lookup (mkAnnKey e1))
      modify $ Map.insert key (annNone {annEntryDelta = annEntryDelta opAnn, annsDP = annsDP negAnns})
      modify $ Map.delete (mkAnnKey e1)
      return res
  where
    loc' = GHC.combineLocs neg_arg e2
    (nofix_error, associate_right) = GHC.compareFixity GHC.negateFixity fix2

---------------------------
--      e1 `op` - neg_arg
mkOpAppRn _ loc e1 op1 fix1 e2@(GHC.L _ GHC.NegApp {}) -- NegApp can occur on the right
  | not associate_right -- We *want* right association
    =
    return $ GHC.L loc (GHC.OpApp noExt e1 op1 e2)
  where
    (_, associate_right) = GHC.compareFixity fix1 GHC.negateFixity

---------------------------
--      Default case
mkOpAppRn _ loc e1 op _fix e2 -- Default case, no rearrangment
  =
  return $ GHC.L loc (GHC.OpApp noExt e1 op e2)

findFixity :: [(String, GHC.Fixity)] -> Expr -> GHC.Fixity
findFixity fs r = askFix fs (getIdent r)

askFix :: [(String, GHC.Fixity)] -> String -> GHC.Fixity
askFix xs = \k -> lookupWithDefault GHC.defaultFixity k xs
  where
    lookupWithDefault def_v k mp1 = fromMaybe def_v $ lookup k mp1

-- | All fixities defined in the Prelude.
preludeFixities :: [(String, GHC.Fixity)]
preludeFixities =
  concat
    [ infixr_ 9 ["."],
      infixl_ 9 ["!!"],
      infixr_ 8 ["^", "^^", "**"],
      infixl_ 7 ["*", "/", "quot", "rem", "div", "mod", ":%", "%"],
      infixl_ 6 ["+", "-"],
      infixr_ 5 [":", "++"],
      infix_ 4 ["==", "/=", "<", "<=", ">=", ">", "elem", "notElem"],
      infixr_ 3 ["&&"],
      infixr_ 2 ["||"],
      infixl_ 1 [">>", ">>="],
      infixr_ 1 ["=<<"],
      infixr_ 0 ["$", "$!", "seq"]
    ]

-- | All fixities defined in the base package.
--
--   Note that the @+++@ operator appears in both Control.Arrows and
--   Text.ParserCombinators.ReadP. The listed precedence for @+++@ in
--   this list is that of Control.Arrows.
baseFixities :: [(String, GHC.Fixity)]
baseFixities =
  preludeFixities
    ++ concat
      [ infixl_ 9 ["!", "//", "!:"],
        infixl_ 8 ["shift", "rotate", "shiftL", "shiftR", "rotateL", "rotateR"],
        infixl_ 7 [".&."],
        infixl_ 6 ["xor"],
        infix_ 6 [":+"],
        infixl_ 5 [".|."],
        infixr_ 5 ["+:+", "<++", "<+>"], -- fixity conflict for +++ between ReadP and Arrow
        infix_ 5 ["\\\\"],
        infixl_ 4 ["<$>", "<$", "<*>", "<*", "*>", "<**>"],
        infix_ 4 ["elemP", "notElemP"],
        infixl_ 3 ["<|>"],
        infixr_ 3 ["&&&", "***"],
        infixr_ 2 ["+++", "|||"],
        infixr_ 1 ["<=<", ">=>", ">>>", "<<<", "^<<", "<<^", "^>>", ">>^"],
        infixl_ 0 ["on"],
        infixr_ 0 ["par", "pseq"]
      ]

infixr_, infixl_, infix_ :: Int -> [String] -> [(String, GHC.Fixity)]
infixr_ = fixity GHC.InfixR
infixl_ = fixity GHC.InfixL
infix_ = fixity GHC.InfixN

-- Internal: help function for the above definitions.
fixity :: GHC.FixityDirection -> Int -> [String] -> [(String, GHC.Fixity)]
fixity a p = map (,Fixity (SourceText "") p a)
