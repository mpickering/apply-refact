{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

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
    -- AnnKeyMap,
    pattern RealSrcLoc',
    pattern RealSrcSpan',

    -- * Monad
    M,

    -- * Utility
    -- mergeAnns,
    modifyAnnKey,
    replaceAnnKey,
    getAnnSpan,
    getAnnSpanA,
    toGhcSrcSpan,
    toGhcSrcSpan',
    annSpanToSrcSpan,
    srcSpanToAnnSpan,
    setAnnSpanFile,
    setSrcSpanFile,
    setRealSrcSpanFile,
    findParent,
    foldAnnKey,
  )
where

import Control.Monad.Trans.State.Strict (StateT, gets, modify')
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor (bimap)
import Data.Data
  ( Data (gmapQi, toConstr),
    Proxy (..),
    splitTyConApp,
    typeOf,
    typeRep,
    typeRepTyCon,
  )
import Data.Generics.Schemes (something)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Typeable (Typeable, eqT, (:~:) (Refl))
import qualified GHC
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types
import Refact.Compat
  ( -- AnnKeyMap,
    AnnKeywordId (..),
    AnnSpan,
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
  )
import qualified Refact.Types as R
import Unsafe.Coerce (unsafeCoerce)
import Language.Haskell.GHC.ExactPrint.Utils (showAst)

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

-- | Replaces an old expression with a new expression
--
-- Note that usually, new, inp and parent are all the same.
-- replace ::
--   AnnKey -> -- The thing we are replacing
--   AnnKey -> -- The thing which has the annotations we need for the new thing
--   AnnKey -> -- The thing which is going to be inserted
--   AnnKey -> -- The "parent", the largest thing which has he same SrcSpan
--   -- Usually the same as inp and new
--   Anns ->
--   Maybe Anns
replace old new inp parent anns = do
  -- oldan <- Map.lookup old anns
  -- newan <- Map.lookup new anns
  -- oldDelta <- annEntryDelta <$> Map.lookup parent anns
  -- return $ Map.insert inp (combine oldDelta new oldan newan) anns
  undefined

-- combine :: DeltaPos -> AnnKey -> Annotation -> Annotation -> Annotation
combine oldDelta newkey oldann newann =
  -- Ann
  --   { annEntryDelta = newEntryDelta,
  --     annPriorComments = annPriorComments oldann ++ annPriorComments newann,
  --     annFollowingComments = annFollowingComments oldann ++ annFollowingComments newann,
  --     annsDP = removeComma (annsDP newann) ++ extraComma (annsDP oldann),
  --     annSortKey = annSortKey newann,
  --     annCapturedSpan = annCapturedSpan newann
  --   }
  -- where
  --   -- Get rid of structural information when replacing, we assume that the
  --   -- structural information is already there in the new expression.
  --   removeComma =
  --     filter
  --       ( \(kw, _) -> case kw of
  --           G AnnComma
  --             | AnnKey _ (CN "ArithSeq") <- newkey -> True
  --             | otherwise -> False
  --           AnnSemiSep -> False
  --           _ -> True
  --       )

  --   -- Make sure to keep structural information in the template.
  --   extraComma [] = []
  --   extraComma (last -> x) = case fst x of
  --     G AnnComma -> [x]
  --     AnnSemiSep -> [x]
  --     G AnnSemi -> [x]
  --     _ -> []

  --   -- Keep the same delta if moving onto a new row
  --   newEntryDelta
  --     | deltaRow oldDelta > 0 = oldDelta
  --     | otherwise = annEntryDelta oldann
  undefined

-- | A parent in this case is an element which has the same SrcSpan
-- findParent :: Data a => AnnSpan -> Anns -> a -> Maybe AnnKey
-- findParent ss as = something (findParentWorker ss as)
findParent ss as = undefined

-- Note that a parent must also have an annotation.
-- findParentWorker ::
--   forall a.
--   (Data a) =>
--   AnnSpan ->
--   Anns ->
--   a ->
--   Maybe AnnKey
findParentWorker oldSS as a
  -- | con == typeRepTyCon (typeRep (Proxy :: Proxy (GHC.Located GHC.RdrName))) && x == typeRep (Proxy :: Proxy AnnSpan) =
  --   if ss == oldSS
  --     && isJust (Map.lookup (AnnKey ss cn) as)
  --     then Just $ AnnKey ss cn
  --     else Nothing
  -- | otherwise = Nothing
  -- where
  --   (con, ~[x, _]) = splitTyConApp (typeOf a)
  --   ss :: AnnSpan
  --   ss = gmapQi 0 unsafeCoerce a
  --   cn = gmapQi 1 (CN . show . toConstr) a
  = undefined

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
modifyAnnKey m e1 e2 = do
  -- as <- gets fst
  -- let parentKey = fromMaybe (mkAnnKey e2) (findParent (getAnnSpan e2) as m)
  -- e2
  --   <$ modify'
  --     ( bimap
  --         ( dropContextParens e1 e2
  --             . recoverBackquotes e1 e2
  --             . replaceAnnKey (mkAnnKey e1) (mkAnnKey e2) (mkAnnKey e2) parentKey
  --         )
  --         (Map.insertWith (++) (mkAnnKey e1) [mkAnnKey e2])
  --     )
  -- liftIO $ putStrLn $ "modifyAnnKey:m" ++ showAst m

  -- AZ: At this stage e1 is the template of replacement, e2 is the
  -- thing being replaced. We need to copy the top level annotations
  -- from e2 to e1

  liftIO $ putStrLn $ "modifyAnnKey:e1" ++ showAst e1
  liftIO $ putStrLn $ "modifyAnnKey:e2" ++ showAst e2
  let (e2',_,_) = runTransform $ transferEntryDP e1 e2
  liftIO $ putStrLn $ "modifyAnnKey:e2'" ++ showAst e2'
  return e2'

-- | When parens are removed for the entire context, e.g.,
--
-- @
--    - f :: (HasCallStack) => ...
--    + f :: HasCallStack => ...
-- @
--
-- We need to remove the `AnnOpenP` and `AnnCloseP` from the corresponding `Annotation`.
-- dropContextParens ::
--   forall old new.
--   (Typeable old, Typeable new) =>
--   GHC.Located old ->
--   GHC.Located new ->
--   Anns ->
--   Anns
dropContextParens old new anns
  -- | Just Refl <- eqT @old @(GHC.HsType GHC.GhcPs),
  --   Just Refl <- eqT @new @(GHC.HsType GHC.GhcPs),
  --   isParTy old,
  --   not (isParTy new),
  --   Just annOld <- Map.lookup key anns,
  --   (G AnnOpenP, _) : (G AnnCloseP, _) : rest <- annsDP annOld =
  --   Map.adjust (\x -> x {annsDP = rest}) key anns
  -- | otherwise = anns
  -- where
  --   key = AnnKey (getAnnSpan old) (CN "(:)")
  --   isParTy = \case (GHC.L _ GHC.HsParTy {}) -> True; _ -> False
  = undefined

-- | When the template contains a backquoted substitution variable, but the substitute
-- is not backquoted, we must add the corresponding 'GHC.AnnBackQuote's.
--
-- See tests/examples/Backquotes.hs for an example.
-- recoverBackquotes :: GHC.Located old -> GHC.Located new -> Anns -> Anns
recoverBackquotes (getAnnSpan -> old) (getAnnSpan -> new) anns
  -- | Just annOld <- Map.lookup (AnnKey old (CN "Unqual")) anns,
  --   ( (G AnnBackquote, DP (i, j))
  --       : rest@( (G AnnVal, _)
  --                  : (G AnnBackquote, _)
  --                  : _
  --                )
  --     ) <-
  --     annsDP annOld =
  --   let f annNew = case annsDP annNew of
  --         [(G AnnVal, DP (i', j'))] ->
  --           annNew {annsDP = (G AnnBackquote, DP (i + i', j + j')) : rest}
  --         _ -> annNew
  --    in Map.adjust f (AnnKey new (CN "Unqual")) anns
  -- | otherwise = anns
  = undefined

-- | Lower level version of @modifyAnnKey@
-- replaceAnnKey :: AnnKey -> AnnKey -> AnnKey -> AnnKey -> Anns -> Anns
replaceAnnKey old new inp deltainfo a =
  -- fromMaybe a (replace old new inp deltainfo a)
  undefined

-- | Convert a @Refact.Types.SrcSpan@ to a @SrcLoc.SrcSpan@
toGhcSrcSpan :: FilePath -> R.SrcSpan -> GHC.SrcSpan
toGhcSrcSpan = toGhcSrcSpan' . mkFastString

-- | Convert a @Refact.Types.SrcSpan@ to a @SrcLoc.SrcSpan@
toGhcSrcSpan' :: FastString -> R.SrcSpan -> GHC.SrcSpan
toGhcSrcSpan' file R.SrcSpan {..} = GHC.mkSrcSpan (f startLine startCol) (f endLine endCol)
  where
    f = GHC.mkSrcLoc file

-- foldAnnKey ::
--   forall a.
--   (AnnKey -> a) ->
--   (GHC.RealSrcSpan -> AnnConName -> a) ->
--   AnnKey ->
--   a
-- foldAnnKey f g key@(AnnKey (annSpanToSrcSpan -> ss) con) = case ss of
--   RealSrcSpan' r -> g r con
--   _ -> f key
foldAnnKey f g key = undefined
