{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Refact.Utils ( -- * Synonyms
                      Stmt
                    , Expr
                    , Decl
                    , Name
                    , Pat
                    , Type
                    , Import
                    , AnnKeyMap
                    -- * Monad
                    , M
                    -- * Utility

                    , mergeAnns
                    , modifyAnnKey
                    , replaceAnnKey
                    , getAnnSpan
                    , toGhcSrcSpan
                    , toGhcSrcSpan'
                    , findParent
                    , foldAnnKey,
                    ) where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types

import Data.Bifunctor (bimap)
import Data.Data
    ( Data(toConstr, gmapQi),
      typeRep,
      Proxy(..),
      splitTyConApp,
      typeOf,
      typeRepTyCon )

import qualified GHC

import Control.Monad.Trans.State ( StateT, gets, modify )

import qualified Data.Map.Strict as Map
import Data.Maybe ( fromMaybe, isJust )


import qualified Refact.Types as R

import Data.Generics.Schemes ( something )
import Unsafe.Coerce ( unsafeCoerce )

import Refact.Compat
  (AnnKeyMap, AnnKeywordId (..), srcSpanToAnnSpan,
   mkFastString, FastString, pattern RealSrcSpan', annSpanToSrcSpan,
  )

-- Types
type M a = StateT (Anns, AnnKeyMap) IO a

type Expr = GHC.Located (GHC.HsExpr GHC.GhcPs)

type Type = GHC.Located (GHC.HsType GHC.GhcPs)

type Decl = GHC.Located (GHC.HsDecl GHC.GhcPs)

type Pat =  GHC.Located (GHC.Pat GHC.GhcPs)

type Name = GHC.Located GHC.RdrName

type  Stmt = GHC.ExprLStmt GHC.GhcPs

type Import = GHC.LImportDecl GHC.GhcPs


-- | Replaces an old expression with a new expression
--
-- Note that usually, new, inp and parent are all the same.
replace :: AnnKey  -- The thing we are replacing
        -> AnnKey  -- The thing which has the annotations we need for the new thing
        -> AnnKey  -- The thing which is going to be inserted
        -> AnnKey  -- The "parent", the largest thing which has he same SrcSpan
                   -- Usually the same as inp and new
        -> Anns -> Maybe Anns
replace old new inp parent anns = do
  oldan <- Map.lookup old anns
  newan <- Map.lookup new anns
  oldDelta <- annEntryDelta  <$> Map.lookup parent anns
  return $ Map.insert inp (combine oldDelta new oldan newan) anns

combine :: DeltaPos -> AnnKey -> Annotation -> Annotation -> Annotation
combine oldDelta newkey oldann newann =
  Ann { annEntryDelta = newEntryDelta
      , annPriorComments = annPriorComments oldann ++ annPriorComments newann
      , annFollowingComments = annFollowingComments oldann ++ annFollowingComments newann
      , annsDP = removeComma (annsDP newann) ++ extraComma (annsDP oldann)
      , annSortKey = annSortKey newann
      , annCapturedSpan = annCapturedSpan newann}
  where
    -- Get rid of structural information when replacing, we assume that the
    -- structural information is already there in the new expression.
    removeComma = filter (\(kw, _) -> case kw of
                                         G AnnComma
                                           | AnnKey _ (CN "ArithSeq") <- newkey -> True
                                           | otherwise -> False
                                         AnnSemiSep -> False
                                         _ -> True)

    -- Make sure to keep structural information in the template.
    extraComma [] = []
    extraComma (last -> x) = case fst x of
                              G AnnComma -> [x]
                              AnnSemiSep -> [x]
                              G AnnSemi -> [x]
                              _ -> []

    -- Keep the same delta if moving onto a new row
    newEntryDelta | deltaRow oldDelta > 0 = oldDelta
                  | otherwise = annEntryDelta oldann


-- | A parent in this case is an element which has the same SrcSpan
findParent :: Data a => AnnSpan -> Anns -> a -> Maybe AnnKey
findParent ss as = something (findParentWorker ss as)

-- Note that a parent must also have an annotation.
findParentWorker :: forall a . (Data a)
           => AnnSpan -> Anns -> a -> Maybe AnnKey
findParentWorker oldSS as a
  | con == typeRepTyCon (typeRep (Proxy :: Proxy (GHC.Located GHC.RdrName))) && x == typeRep (Proxy :: Proxy AnnSpan)
      = if ss == oldSS
            && isJust (Map.lookup (AnnKey ss cn) as)
          then Just $ AnnKey ss cn
          else Nothing
  | otherwise = Nothing
  where
    (con, ~[x, _]) = splitTyConApp (typeOf a)
    ss :: AnnSpan
    ss = gmapQi 0 unsafeCoerce a
    cn = gmapQi 1 (CN . show . toConstr) a

getAnnSpan :: forall a. GHC.Located a -> AnnSpan
getAnnSpan = srcSpanToAnnSpan . GHC.getLoc

-- | Perform the necessary adjustments to annotations when replacing
-- one Located thing with another Located thing.
--
-- For example, this function will ensure the correct relative position and
-- make sure that any trailing semi colons or commas are transferred.
modifyAnnKey
  :: (Data old, Data new, Data mod)
  => mod -> GHC.Located old -> GHC.Located new -> M (GHC.Located new)
modifyAnnKey m e1 e2 = do
  as <- gets fst
  let parentKey = fromMaybe (mkAnnKey e2) (findParent (getAnnSpan e2) as m)
  e2 <$ modify
          ( bimap
              ( recoverBackquotes e1 e2
              . replaceAnnKey (mkAnnKey e1) (mkAnnKey e2) (mkAnnKey e2) parentKey
              )
              (Map.insertWith (++) (mkAnnKey e1) [mkAnnKey e2])
          )

-- | When the template contains a backquoted substitution variable, but the substitute
-- is not backquoted, we must add the corresponding 'GHC.AnnBackQuote's.
--
-- See tests/examples/Backquotes.hs for an example.
recoverBackquotes :: GHC.Located old -> GHC.Located new -> Anns -> Anns
recoverBackquotes (getAnnSpan -> old) (getAnnSpan -> new) anns
  | Just annOld <- Map.lookup (AnnKey old (CN "Unqual")) anns
  , ( (G AnnBackquote, DP (i, j))
    : rest@( (G AnnVal, _)
           : (G AnnBackquote, _)
           : _)
    ) <- annsDP annOld
  = let f annNew = case annsDP annNew of
          [(G AnnVal, DP (i', j'))] ->
            annNew {annsDP = (G AnnBackquote, DP (i + i', j + j')) : rest}
          _ -> annNew
     in Map.adjust f (AnnKey new (CN "Unqual")) anns
  | otherwise = anns

-- | Lower level version of @modifyAnnKey@
replaceAnnKey :: AnnKey -> AnnKey -> AnnKey -> AnnKey -> Anns -> Anns
replaceAnnKey old new inp deltainfo a =
  fromMaybe a (replace old new inp deltainfo a)

-- | Convert a @Refact.Types.SrcSpan@ to a @SrcLoc.SrcSpan@
toGhcSrcSpan :: FilePath -> R.SrcSpan -> GHC.SrcSpan
toGhcSrcSpan = toGhcSrcSpan' . mkFastString

-- | Convert a @Refact.Types.SrcSpan@ to a @SrcLoc.SrcSpan@
toGhcSrcSpan' :: FastString -> R.SrcSpan -> GHC.SrcSpan
toGhcSrcSpan' file R.SrcSpan{..} = GHC.mkSrcSpan (f startLine startCol) (f endLine endCol)
  where
    f = GHC.mkSrcLoc file

foldAnnKey ::
  forall a.
  (AnnKey -> a) ->
  (GHC.RealSrcSpan -> AnnConName -> a) ->
  AnnKey ->
  a
foldAnnKey f g key@(AnnKey (annSpanToSrcSpan -> ss) con) = case ss of
  RealSrcSpan' r -> g r con
  _ -> f key
