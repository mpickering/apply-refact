{-# LANGUAGE ViewPatterns #-}

module Refact.Fixity (applyFixities) where

import Control.Monad.Trans.State
import Data.Generics hiding (Fixity)
import Data.Maybe
import qualified GHC
import Language.Haskell.GHC.ExactPrint
import Refact.Compat (Fixity (..), SourceText (..), occNameString, rdrNameOcc)
import Refact.Utils

-- | Rearrange infix expressions to account for fixity.
-- The set of fixities is wired in and includes all fixities in base.
applyFixities :: Module -> IO Module
applyFixities m = fst <$> runStateT (everywhereM (mkM expFix) m) ()
  -- Note: everywhereM is a bottom-up transformation

expFix :: Expr -> StateT () IO Expr
expFix (GHC.L loc (GHC.OpApp an l op r)) =
  mkOpAppRn baseFixities loc an l op (findFixity baseFixities op) r
expFix e = return e

getIdent :: Expr -> String
getIdent (GHC.unLoc -> GHC.HsVar _ (GHC.L _ n)) = occNameString . rdrNameOcc $ n
getIdent _ = error "Must be HsVar"

---------------------------
-- Modified from GHC Renamer
mkOpAppRn ::
  [(String, GHC.Fixity)] ->
  GHC.SrcSpanAnnA ->
  GHC.EpAnn [GHC.AddEpAnn] ->
  Expr -> -- Left operand; already rearranged
  Expr ->
  GHC.Fixity -> -- Operator and fixity
  Expr -> -- Right operand (not an OpApp, but might
  -- be a NegApp)
  StateT () IO Expr
-- (e11 `op1` e12) `op2` e2
mkOpAppRn fs loc an e1@(GHC.L _ (GHC.OpApp x1 e11 op1 e12)) op2 fix2 e2
  | nofix_error =
    return $ GHC.L loc (GHC.OpApp an e1 op2 e2)
  | associate_right = do
    -- liftIO $ putStrLn $ "mkOpAppRn:1:e1" ++ showAst e1
    let e12' = setEntryDP e12 (GHC.SameLine 0)
    new_e <- mkOpAppRn fs loc' an e12' op2 fix2 e2
    let (new_e',_,_) = runTransform $ transferEntryDP e12 new_e
    let res = GHC.L loc (GHC.OpApp x1 e11 op1 new_e')
    -- liftIO $ putStrLn $ "mkOpAppRn:1:res" ++ showAst res
    return res
  where
    loc' = GHC.combineLocsA e12 e2
    fix1 = findFixity fs op1
    (nofix_error, associate_right) = GHC.compareFixity fix1 fix2

---------------------------
--      (- neg_arg) `op` e2
mkOpAppRn fs loc an e1@(GHC.L _ (GHC.NegApp an' neg_arg neg_name)) op2 fix2 e2
  | nofix_error =
    return (GHC.L loc (GHC.OpApp an e1 op2 e2))
  | associate_right =
    do
      -- liftIO $ putStrLn $ "mkOpAppRn:2:e1" ++ showAst e1
      new_e <- mkOpAppRn fs loc' an neg_arg op2 fix2 e2
      let new_e' = setEntryDP new_e (GHC.SameLine 0)
      let res = setEntryDP (GHC.L loc (GHC.NegApp an' new_e' neg_name)) (GHC.SameLine 0)
      -- liftIO $ putStrLn $ "mkOpAppRn:2:res" ++ showAst res
      return res
  where
    loc' = GHC.combineLocsA neg_arg e2
    (nofix_error, associate_right) = GHC.compareFixity GHC.negateFixity fix2

---------------------------
--      e1 `op` - neg_arg
mkOpAppRn _ loc an e1 op1 fix1 e2@(GHC.L _ GHC.NegApp {}) -- NegApp can occur on the right
  | not associate_right -- We *want* right association
    = do
    -- liftIO $ putStrLn $ "mkOpAppRn:3:e1" ++ showAst (GHC.L loc (GHC.OpApp an e1 op1 e2))
    return $ GHC.L loc (GHC.OpApp an e1 op1 e2)
  where
    (_, associate_right) = GHC.compareFixity fix1 GHC.negateFixity

---------------------------
--      Default case
mkOpAppRn _ loc an e1 op _fix e2 -- Default case, no rearrangment
  = do
  -- liftIO $ putStrLn $ "mkOpAppRn:4:e1" ++ showAst (GHC.L loc (GHC.OpApp an e1 op e2))
  return $ GHC.L loc (GHC.OpApp an e1 op e2)

-- ---------------------------------------------------------------------

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
