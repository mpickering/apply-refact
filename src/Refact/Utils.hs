{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Refact.Utils
  ( -- * Synonyms
    Module,
    Stmt,
    Expr,
    Decl,
    Name,
    Pat,
    Type,
    Import,
    FunBind,
    pattern RealSrcLoc',
    pattern RealSrcSpan',

    -- * Monad
    M,

    -- * Utility
    modifyAnnKey,
    getAnnSpan,
    getAnnSpanA,
    toGhcSrcSpan,
    toGhcSrcSpan',
    annSpanToSrcSpan,
    srcSpanToAnnSpan,
    setAnnSpanFile,
    setSrcSpanFile,
    setRealSrcSpanFile,
  )
where

import Control.Monad.Trans.State.Strict (StateT)
import Data.Data
  ( Data (),
  )
import Data.Generics (everywhere, mkT)
import Data.Typeable
import qualified GHC
#if MIN_VERSION_ghc(9,12,0)
import Language.Haskell.GHC.ExactPrint hiding (transferEntryDP)
#else
import Language.Haskell.GHC.ExactPrint
#endif
import Refact.Compat
  ( AnnSpan,
    FastString,
    FunBind,
    Module,
    annSpanToSrcSpan,
    mkFastString,
    setAnnSpanFile,
    setRealSrcSpanFile,
    setSrcSpanFile,
    srcSpanToAnnSpan,
    pattern RealSrcLoc',
    pattern RealSrcSpan',
#if MIN_VERSION_ghc(9,12,0)
    transferEntryDP,
#endif
  )
import qualified Refact.Types as R


-- Types
-- type M a = StateT (Anns, AnnKeyMap) IO a
type M a = StateT () IO a

type Expr = GHC.LHsExpr GHC.GhcPs

type Type = GHC.LHsType GHC.GhcPs

type Decl = GHC.LHsDecl GHC.GhcPs

type Pat = GHC.LPat GHC.GhcPs

type Name = GHC.LocatedN GHC.RdrName

type Stmt = GHC.ExprLStmt GHC.GhcPs

type Import = GHC.LImportDecl GHC.GhcPs

getAnnSpanA :: forall an a. GHC.LocatedAn an a -> AnnSpan
getAnnSpanA = srcSpanToAnnSpan . GHC.getLocA

getAnnSpan :: forall a. GHC.Located a -> AnnSpan
getAnnSpan = srcSpanToAnnSpan . GHC.getLoc

-- | Perform the necessary adjustments to annotations when replacing
-- one Located thing with another Located thing.
--
-- For example, this function will ensure the correct relative position and
-- make sure that any trailing semi colons or commas are transferred.
-- modifyAnnKey ::
--   (Data old, Data new, Data mod) =>
--   mod ->
--   GHC.Located old ->
--   GHC.Located new ->
--   M (GHC.Located new)
modifyAnnKey ::
  (Data mod, Data t, Data old, Data new, Monoid t, Typeable t) =>
  mod ->
  GHC.LocatedAn t old ->
  GHC.LocatedAn t new ->
  M (GHC.LocatedAn t new)
modifyAnnKey _m e1 e2 = do
  -- liftIO $ putStrLn $ "modifyAnnKey:e1" ++ showAst e1
  -- liftIO $ putStrLn $ "modifyAnnKey:e2" ++ showAst e2
  let e2_0 = handleBackquotes e1 e2
  -- liftIO $ putStrLn $ "modifyAnnKey:e2_0" ++ showAst e2_0
  let (e2', _, _) = runTransform $ transferEntryDP e1 e2_0
  -- liftIO $ putStrLn $ "modifyAnnKey:e2'" ++ showAst e2'
  return e2'

-- | This function handles backquotes in two scenarios:
--
--     1. When the template contains a backquoted substitution variable, but the substitute
--        is not backquoted, we must add the corresponding 'GHC.NameBackquotes'. See
--        tests/examples/Backquotes.hs for an example.
--     2. When the template contains a substitution variable without backquote, and the
--        substitute is backquoted, we remove the 'GHC.NameBackquotes' annotation. See
--        tests/examples/Uncurry.hs for an example.
--        N.B.: this is not always correct, since it is possible that the refactoring output
--        should keep the backquotes, but currently no test case fails because of it.
handleBackquotes ::
  forall t old new.
  (Data t, Data old, Data new, Monoid t, Typeable t) =>
  GHC.LocatedAn t old ->
  GHC.LocatedAn t new ->
  GHC.LocatedAn t new
#if MIN_VERSION_ghc(9,12,0)
handleBackquotes old new@(GHC.L loc _) =
  everywhere (mkT update) new
  where
    update :: GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
    update (GHC.L l (GHC.HsVar x (GHC.L ln n))) = GHC.L l (GHC.HsVar x (GHC.L ln' n))
      where
        ln' =
          if GHC.locA l == GHC.locA loc
            then case cast old :: Maybe (GHC.LHsExpr GHC.GhcPs) of
              Just (GHC.L _ (GHC.HsVar _ (GHC.L (GHC.EpAnn _ ann _) _)))
                -- scenario 1
                | GHC.NameAnn (GHC.NameBackquotes _ _) _ _ <- ann ->
                  case ln of
                    (GHC.EpAnn a _ cs) -> (GHC.EpAnn a ann cs)
                -- scenario 2
                | (GHC.EpAnn a ann' cs) <- ln,
                  GHC.NameAnn (GHC.NameBackquotes _ _) _ _ <- ann' ->
                  (GHC.EpAnn a ann cs)
              Just _ -> ln
              Nothing -> ln
            else ln
    update x = x
#else
handleBackquotes old new@(GHC.L loc _) =
  everywhere (mkT update) new
  where
    update :: GHC.LHsExpr GHC.GhcPs -> GHC.LHsExpr GHC.GhcPs
    update (GHC.L l (GHC.HsVar x (GHC.L ln n))) = GHC.L l (GHC.HsVar x (GHC.L ln' n))
      where
        ln' =
          if GHC.locA l == GHC.locA loc
            then case cast old :: Maybe (GHC.LHsExpr GHC.GhcPs) of
              Just (GHC.L _ (GHC.HsVar _ (GHC.L (GHC.SrcSpanAnn (GHC.EpAnn _ ann _) _) _)))
                -- scenario 1
                | GHC.NameAnn GHC.NameBackquotes _ _ _ _ <- ann ->
                  case ln of
                    (GHC.SrcSpanAnn (GHC.EpAnn a _ cs) ll) -> GHC.SrcSpanAnn (GHC.EpAnn a ann cs) ll
                    (GHC.SrcSpanAnn GHC.EpAnnNotUsed ll) ->
                      GHC.SrcSpanAnn (GHC.EpAnn (GHC.spanAsAnchor ll) ann GHC.emptyComments) ll
                -- scenario 2
                | GHC.SrcSpanAnn (GHC.EpAnn a ann' cs) ll <- ln,
                  GHC.NameAnn GHC.NameBackquotes _ _ _ _ <- ann' ->
                  GHC.SrcSpanAnn (GHC.EpAnn a ann cs) ll
              Just _ -> ln
              Nothing -> ln
            else ln
    update x = x
#endif

-- | Convert a @Refact.Types.SrcSpan@ to a @SrcLoc.SrcSpan@
toGhcSrcSpan :: FilePath -> R.SrcSpan -> GHC.SrcSpan
toGhcSrcSpan = toGhcSrcSpan' . mkFastString

-- | Convert a @Refact.Types.SrcSpan@ to a @SrcLoc.SrcSpan@
toGhcSrcSpan' :: FastString -> R.SrcSpan -> GHC.SrcSpan
toGhcSrcSpan' file R.SrcSpan {..} = GHC.mkSrcSpan (f startLine startCol) (f endLine endCol)
  where
    f = GHC.mkSrcLoc file
