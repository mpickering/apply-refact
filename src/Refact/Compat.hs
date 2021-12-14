{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Refact.Compat (
  -- * ApiAnnotation / GHC.Parser.ApiAnnotation
  AnnKeywordId (..),

  -- * BasicTypes / GHC.Types.Basic
  Fixity (..),
  SourceText (..),

  -- * DynFlags / GHC.Driver.Session
  FlagSpec (..),
  GeneralFlag (..),
  gopt_set,
  gopt_unset,
  parseDynamicFilePragma,
  xopt_set,
  xopt_unset,
  xFlags,

  -- * ErrUtils
  Errors,
  ErrorMessages,
  onError,
  pprErrMsgBagWithLoc,

  -- * FastString / GHC.Data.FastString
  FastString,
  mkFastString,

  -- * HeaderInfo / GHC.Parser.Header
  getOptions,

  -- * HsExpr / GHC.Hs.Expr
  GRHS (..),
  HsExpr (..),
  HsMatchContext (..),
  HsStmtContext (..),
  Match (..),
  MatchGroup (..),
  StmtLR (..),

  -- * HsSyn / GHC.Hs
#if __GLASGOW_HASKELL__ >= 810
  module GHC.Hs,
#else
  module HsSyn,
#endif

  -- * Name / OccName / GHC.Types.Name
  nameOccName,
  occName,
  occNameString,
  ppr,

  -- * Outputable / GHC.Utils.Outputable
  showSDocUnsafe,

  -- * Panic / GHC.Utils.Panic
  handleGhcException,

  -- * RdrName / GHC.Types.Name.Reader
  RdrName (..),
  rdrNameOcc,

  -- * SrcLoc / GHC.Types.SrcLoc
  GenLocated (..),
  pattern RealSrcLoc',
  pattern RealSrcSpan',
  RealSrcSpan (..),
  SrcSpanLess,
  combineSrcSpans,
  composeSrcSpan,
  decomposeSrcSpan,

  -- * StringBuffer
  stringToStringBuffer,

  -- * Misc
  impliedXFlags,

  -- * Non-GHC stuff
  AnnKeyMap,
  FunBind,
  DoGenReplacement,
  Module,
  MonadFail',
  ReplaceWorker,
  annSpanToSrcSpan,
  badAnnSpan,
  mkErr,
  parseModuleName,
  setAnnSpanFile,
  setRealSrcSpanFile,
  setSrcSpanFile,
  srcSpanToAnnSpan,
) where

#if __GLASGOW_HASKELL__ >= 900
import GHC.Data.Bag (unitBag)
import GHC.Data.FastString (FastString, mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Session hiding (initDynFlags)
import GHC.Parser.Annotation
import GHC.Parser.Header (getOptions)
import GHC.Types.Basic (Fixity (..), SourceText (..))
import GHC.Types.Name (nameOccName, occName, occNameString)
import GHC.Types.Name.Reader (RdrName (..), rdrNameOcc)
import GHC.Types.SrcLoc hiding (spans)
import GHC.Utils.Error
import GHC.Utils.Outputable
  ( ppr,
    showSDocUnsafe,
    pprPanic,
    text,
    vcat,
  )
import GHC.Utils.Panic (handleGhcException)
#else
import ApiAnnotation
#if __GLASGOW_HASKELL__ == 810
import Bag (unitBag)
#endif
import BasicTypes (Fixity (..), SourceText (..))
import ErrUtils
  ( ErrorMessages,
    pprErrMsgBagWithLoc,
#if __GLASGOW_HASKELL__ == 810
    mkPlainErrMsg,
#endif
  )
import DynFlags hiding (initDynFlags)
import FastString (FastString, mkFastString)
import GHC.LanguageExtensions.Type (Extension (..))
import HeaderInfo (getOptions)
import Name (nameOccName)
import OccName (occName, occNameString)
import Outputable
  ( ppr,
    showSDocUnsafe,
#if __GLASGOW_HASKELL__ == 810
    pprPanic,
    text,
    vcat,
#endif
  )
import Panic (handleGhcException)
import RdrName (RdrName (..), rdrNameOcc)
import SrcLoc hiding (spans)
import StringBuffer (stringToStringBuffer)
#endif

#if __GLASGOW_HASKELL__ >= 810
import GHC.Hs hiding (Pat, Stmt)
#elif __GLASGOW_HASKELL__ <= 808
import HsSyn hiding (Pat, Stmt)
#endif

import Control.Monad.Trans.State.Strict (StateT)
import Data.Data (Data)
import Data.Map.Strict (Map)
import qualified GHC
import Language.Haskell.GHC.ExactPrint.Delta ( relativiseApiAnns )
import Language.Haskell.GHC.ExactPrint.Parsers (Parser)
import Language.Haskell.GHC.ExactPrint.Types
  ( Anns,
    AnnKey (..),
    AnnSpan,
#if __GLASGOW_HASKELL__ >= 900
    badRealSrcSpan,
#endif
  )
import Refact.Types (Refactoring)

#if __GLASGOW_HASKELL__ <= 806
type MonadFail' = Monad
#else
type MonadFail' = MonadFail
#endif

type AnnKeyMap = Map AnnKey [AnnKey]

#if __GLASGOW_HASKELL__ >= 900
type Module = Located HsModule
#else
type Module = Located (HsModule GhcPs)
#endif

#if __GLASGOW_HASKELL__ >= 810
type Errors = ErrorMessages
onError :: String -> Errors -> a
onError s = pprPanic s . vcat . pprErrMsgBagWithLoc
#else
type Errors = (SrcSpan, String)
onError :: String -> Errors -> a
onError _ = error . show
#endif

#if __GLASGOW_HASKELL__ >= 900
type FunBind = HsMatchContext GhcPs
#else
type FunBind = HsMatchContext RdrName
#endif

pattern RealSrcLoc' :: RealSrcLoc -> SrcLoc
#if __GLASGOW_HASKELL__ >= 900
pattern RealSrcLoc' r <- RealSrcLoc r _ where
  RealSrcLoc' r = RealSrcLoc r Nothing
#else
pattern RealSrcLoc' r <- RealSrcLoc r where
  RealSrcLoc' r = RealSrcLoc r
#endif
{-# COMPLETE RealSrcLoc', UnhelpfulLoc #-}

pattern RealSrcSpan' :: RealSrcSpan -> SrcSpan
#if __GLASGOW_HASKELL__ >= 900
pattern RealSrcSpan' r <- RealSrcSpan r _ where
  RealSrcSpan' r = RealSrcSpan r Nothing
#else
pattern RealSrcSpan' r <- RealSrcSpan r where
  RealSrcSpan' r = RealSrcSpan r
#endif
{-# COMPLETE RealSrcSpan', UnhelpfulSpan #-}

#if __GLASGOW_HASKELL__ <= 806 || __GLASGOW_HASKELL__ >= 900
composeSrcSpan :: a -> a
composeSrcSpan = id

decomposeSrcSpan :: a -> a
decomposeSrcSpan = id

type SrcSpanLess a = a
#endif

badAnnSpan :: AnnSpan
badAnnSpan =
#if __GLASGOW_HASKELL__ >= 900
  badRealSrcSpan
#else
  noSrcSpan
#endif

srcSpanToAnnSpan :: SrcSpan -> AnnSpan
srcSpanToAnnSpan =
#if __GLASGOW_HASKELL__ >= 900
  \case RealSrcSpan l _ -> l; _ -> badRealSrcSpan
#else
  id
#endif

annSpanToSrcSpan :: AnnSpan -> SrcSpan
annSpanToSrcSpan =
#if __GLASGOW_HASKELL__ >= 900
  flip RealSrcSpan Nothing
#else
  id
#endif

setSrcSpanFile :: FastString -> SrcSpan -> SrcSpan
setSrcSpanFile file s
  | RealSrcLoc' start <- srcSpanStart s,
    RealSrcLoc' end <- srcSpanEnd s =
    let start' = mkSrcLoc file (srcLocLine start) (srcLocCol start)
        end' = mkSrcLoc file (srcLocLine end) (srcLocCol end)
     in mkSrcSpan start' end'
setSrcSpanFile _ s = s

setRealSrcSpanFile :: FastString -> RealSrcSpan -> RealSrcSpan
setRealSrcSpanFile file s = mkRealSrcSpan start' end'
  where
    start = realSrcSpanStart s
    end = realSrcSpanEnd s
    start' = mkRealSrcLoc file (srcLocLine start) (srcLocCol start)
    end' = mkRealSrcLoc file (srcLocLine end) (srcLocCol end)

setAnnSpanFile :: FastString -> AnnSpan -> AnnSpan
setAnnSpanFile =
#if __GLASGOW_HASKELL__ >= 900
  setRealSrcSpanFile
#else
  setSrcSpanFile
#endif

mkErr :: DynFlags -> SrcSpan -> String -> Errors
#if __GLASGOW_HASKELL__ >= 810
mkErr df l s = unitBag (mkPlainErrMsg df l (text s))
#else
mkErr = const (,)
#endif

parseModuleName :: SrcSpan -> Parser (Located GHC.ModuleName)
parseModuleName ss _ _ s =
  let newMN =  GHC.L ss (GHC.mkModuleName s)
#if __GLASGOW_HASKELL__ >= 900
      newAnns = relativiseApiAnns newMN (GHC.ApiAnns mempty Nothing mempty mempty)
#else
      newAnns = relativiseApiAnns newMN mempty
#endif
  in pure (newAnns, newMN)

#if __GLASGOW_HASKELL__ <= 806 || __GLASGOW_HASKELL__ >= 900
type DoGenReplacement ast a =
  (Data ast, Data a) =>
  a ->
  (Located ast -> Bool) ->
  Located ast ->
  Located ast ->
  StateT ((Anns, AnnKeyMap), Bool) IO (Located ast)
#else
type DoGenReplacement ast a =
  (Data (SrcSpanLess ast), HasSrcSpan ast, Data a) =>
  a ->
  (ast -> Bool) ->
  ast ->
  ast ->
  StateT ((Anns, AnnKeyMap), Bool) IO ast
#endif

#if __GLASGOW_HASKELL__ <= 806 || __GLASGOW_HASKELL__ >= 900
type ReplaceWorker a mod =
  (Data a, Data mod) =>
  Anns ->
  mod ->
  AnnKeyMap ->
  Parser (Located a) ->
  Int ->
  Refactoring SrcSpan ->
  IO (Anns, mod, AnnKeyMap)
#else
type ReplaceWorker a mod =
  (Data a, HasSrcSpan a, Data mod, Data (SrcSpanLess a)) =>
  Anns ->
  mod ->
  AnnKeyMap ->
  Parser a ->
  Int ->
  Refactoring SrcSpan ->
  IO (Anns, mod, AnnKeyMap)
#endif

#if __GLASGOW_HASKELL__ < 900
-- | Copied from "Language.Haskell.GhclibParserEx.GHC.Driver.Session", in order to
-- support GHC 8.6
impliedXFlags :: [(Extension, Bool, Extension)]
impliedXFlags
-- See Note [Updating flag description in the User's Guide]
  = [ (RankNTypes,                True, ExplicitForAll)
    , (QuantifiedConstraints,     True, ExplicitForAll)
    , (ScopedTypeVariables,       True, ExplicitForAll)
    , (LiberalTypeSynonyms,       True, ExplicitForAll)
    , (ExistentialQuantification, True, ExplicitForAll)
    , (FlexibleInstances,         True, TypeSynonymInstances)
    , (FunctionalDependencies,    True, MultiParamTypeClasses)
    , (MultiParamTypeClasses,     True, ConstrainedClassMethods)  -- c.f. #7854
    , (TypeFamilyDependencies,    True, TypeFamilies)

    , (RebindableSyntax, False, ImplicitPrelude)      -- NB: turn off!

    , (DerivingVia, True, DerivingStrategies)

    , (GADTs,            True, GADTSyntax)
    , (GADTs,            True, MonoLocalBinds)
    , (TypeFamilies,     True, MonoLocalBinds)

    , (TypeFamilies,     True, KindSignatures)  -- Type families use kind signatures
    , (PolyKinds,        True, KindSignatures)  -- Ditto polymorphic kinds

    -- TypeInType is now just a synonym for a couple of other extensions.
    , (TypeInType,       True, DataKinds)
    , (TypeInType,       True, PolyKinds)
    , (TypeInType,       True, KindSignatures)

    -- AutoDeriveTypeable is not very useful without DeriveDataTypeable
    , (AutoDeriveTypeable, True, DeriveDataTypeable)

    -- We turn this on so that we can export associated type
    -- type synonyms in subordinates (e.g. MyClass(type AssocType))
    , (TypeFamilies,     True, ExplicitNamespaces)
    , (TypeOperators, True, ExplicitNamespaces)

    , (ImpredicativeTypes,  True, RankNTypes)

        -- Record wild-cards implies field disambiguation
        -- Otherwise if you write (C {..}) you may well get
        -- stuff like " 'a' not in scope ", which is a bit silly
        -- if the compiler has just filled in field 'a' of constructor 'C'
    , (RecordWildCards,     True, DisambiguateRecordFields)

    , (ParallelArrays, True, ParallelListComp)

    , (JavaScriptFFI, True, InterruptibleFFI)

    , (DeriveTraversable, True, DeriveFunctor)
    , (DeriveTraversable, True, DeriveFoldable)

    -- Duplicate record fields require field disambiguation
    , (DuplicateRecordFields, True, DisambiguateRecordFields)

    , (TemplateHaskell, True, TemplateHaskellQuotes)
    , (Strict, True, StrictData)
#if __GLASGOW_HASKELL__ >= 810
    , (StandaloneKindSignatures, False, CUSKs)
#endif
  ]
#endif
