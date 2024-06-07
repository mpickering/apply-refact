{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Refact.Compat (
  -- * ApiAnnotation / GHC.Parser.ApiAnnotation
  AnnKeywordId (..),
  DeltaPos(..),

  -- * BasicTypes / GHC.Types.Basic
  Fixity(..),
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
  module GHC.Hs,

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
  SrcAnn,
  OpAppAnn,
  combineSrcSpans,
  composeSrcSpan,
  decomposeSrcSpan,
  fromSrcSpanAnn,
  toSrcSpanAnn,
  addCommentsToSrcAnn,
  mkAnchor,
  lEpaCommentRealSrcSpan,
  srcSpanAnnDeltaPos,

  -- * StringBuffer
  stringToStringBuffer,

  -- * Misc
  impliedXFlags,

  -- * Non-GHC stuff
  -- AnnKeyMap,
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
  AnnSpan,

#if MIN_VERSION_ghc(9,4,0)
  -- * GHC 9.4 stuff
  initParserOpts,
#endif

  -- * ghc-exactprint stuff
  makeDeltaAst,
  refactOptions,
  transferEntryDP,
  transferEntryDP'
) where

import Control.Monad.Trans.State.Strict (StateT)
import Data.Data (Data)
import Data.Dynamic (Typeable)
import Data.Functor.Identity (Identity (..))
import qualified GHC
import GHC.Data.Bag (unitBag, bagToList)
import GHC.Data.FastString (FastString, mkFastString)
#if MIN_VERSION_ghc(9,4,0)
import qualified GHC.Data.Strict as Strict
#endif
import GHC.Data.StringBuffer (stringToStringBuffer)
#if MIN_VERSION_ghc(9,4,0)
import GHC.Driver.Config.Parser
import GHC.Driver.Errors.Types (ErrorMessages, ghcUnknownMessage, GhcMessage)
#endif
import GHC.Driver.Session hiding (initDynFlags)
#if MIN_VERSION_ghc(9,10,0)
import GHC.Hs hiding (Pat, Stmt, parseModuleName)
#elif MIN_VERSION_ghc(9,6,0)
import GHC.Hs hiding (Pat, SrcAnn, SrcSpanAnn' (..), Stmt, addCommentsToSrcAnn, parseModuleName)
#else
import GHC.Hs hiding (Pat, SrcAnn, SrcSpanAnn' (..), Stmt, addCommentsToSrcAnn)
#endif
import GHC.Parser.Header (getOptions)
#if MIN_VERSION_ghc(9,8,0)
import GHC.Types.Error (defaultDiagnosticOpts, getMessages)
#elif MIN_VERSION_ghc(9,4,0)
import GHC.Types.Error (getMessages)
#endif
import GHC.Types.Fixity  ( Fixity(..) )
import GHC.Types.Name (nameOccName, occName, occNameString)
import GHC.Types.Name.Reader (RdrName (..), rdrNameOcc)
import GHC.Types.SrcLoc hiding (spans)
import GHC.Types.SourceText
#if MIN_VERSION_ghc(9,4,0)
import GHC.Utils.Error
#else
import GHC.Utils.Error hiding (mkErr)
#endif
import GHC.Utils.Outputable
  ( ppr,
    showSDocUnsafe,
    text,
    vcat,
  )
import GHC.Utils.Panic
  ( handleGhcException
  , pprPanic
  )
import Language.Haskell.GHC.ExactPrint.Parsers (Parser)
import qualified Language.Haskell.GHC.ExactPrint as EP
import qualified Language.Haskell.GHC.ExactPrint.ExactPrint as EP
import qualified Language.Haskell.GHC.ExactPrint.Types as EP
import Language.Haskell.GHC.ExactPrint.Utils
import Refact.Types (Refactoring)

type MonadFail' = MonadFail

#if MIN_VERSION_ghc(9,6,0)
type Module = Located (HsModule GhcPs)
#else
type Module = Located HsModule
#endif

type Errors = ErrorMessages

onError :: String -> Errors -> a
onError s = pprPanic s . vcat . ppp

ppp :: Errors -> [SDoc]
#if MIN_VERSION_ghc(9,6,0)
ppp pst = concatMap unDecorated $ fmap ((diagnosticMessage (defaultDiagnosticOpts @GhcMessage)) . errMsgDiagnostic) $ bagToList $ getMessages pst
#elif MIN_VERSION_ghc(9,4,0)
ppp pst = concatMap unDecorated $ fmap (diagnosticMessage . errMsgDiagnostic) $ bagToList $ getMessages pst
#else
ppp pst = concatMap unDecorated (errMsgDiagnostic <$> bagToList pst)
#endif

#if MIN_VERSION_ghc(9,10,0)
type FunBind = HsMatchContext (LIdP (NoGhcTc GhcPs))
#else
type FunBind = HsMatchContext GhcPs
#endif

pattern RealSrcLoc' :: RealSrcLoc -> SrcLoc
pattern RealSrcLoc' r <- RealSrcLoc r _ where
#if MIN_VERSION_ghc(9,4,0)
  RealSrcLoc' r = RealSrcLoc r Strict.Nothing
#else
  RealSrcLoc' r = RealSrcLoc r Nothing
#endif
{-# COMPLETE RealSrcLoc', UnhelpfulLoc #-}

pattern RealSrcSpan' :: RealSrcSpan -> SrcSpan
pattern RealSrcSpan' r <- RealSrcSpan r _ where
#if MIN_VERSION_ghc(9,4,0)
  RealSrcSpan' r = RealSrcSpan r Strict.Nothing
#else
  RealSrcSpan' r = RealSrcSpan r Nothing
#endif
{-# COMPLETE RealSrcSpan', UnhelpfulSpan #-}

composeSrcSpan :: a -> a
composeSrcSpan = id

decomposeSrcSpan :: a -> a
decomposeSrcSpan = id

type SrcSpanLess a = a

type AnnSpan = RealSrcSpan
badAnnSpan :: AnnSpan
badAnnSpan =
  badRealSrcSpan

srcSpanToAnnSpan :: SrcSpan -> AnnSpan
srcSpanToAnnSpan =
  \case RealSrcSpan l _ -> l; _ -> badRealSrcSpan

annSpanToSrcSpan :: AnnSpan -> SrcSpan
annSpanToSrcSpan =
#if MIN_VERSION_ghc(9,4,0)
  flip RealSrcSpan Strict.Nothing
#else
  flip RealSrcSpan Nothing
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
  setRealSrcSpanFile

#if MIN_VERSION_ghc(9,10,0)
toSrcSpanAnn :: GHC.EpAnn ann -> GHC.SrcSpan -> GHC.EpAnn ann
toSrcSpanAnn ann = const ann
#else
toSrcSpanAnn :: GHC.EpAnn ann -> GHC.SrcSpan -> GHC.SrcAnn ann
toSrcSpanAnn = GHC.SrcSpanAnn
#endif

#if MIN_VERSION_ghc(9,10,0)
fromSrcSpanAnn :: GHC.EpAnn ann -> (GHC.EpAnn ann, GHC.SrcSpan)
fromSrcSpanAnn ann = (ann, UnhelpfulSpan UnhelpfulNoLocationInfo)
#else
fromSrcSpanAnn :: GHC.SrcAnn ann -> (GHC.EpAnn ann, GHC.SrcSpan)
fromSrcSpanAnn (GHC.SrcSpanAnn ann s) = (ann, s)
#endif

#if MIN_VERSION_ghc(9,10,0)
srcSpanAnnDeltaPos :: GHC.EpAnn ann -> Maybe GHC.DeltaPos
srcSpanAnnDeltaPos = \case
  GHC.EpAnn (GHC.EpaDelta dp _) _ _ -> Just dp
  _ -> Nothing
#else
srcSpanAnnDeltaPos :: GHC.SrcAnn ann -> Maybe GHC.DeltaPos
srcSpanAnnDeltaPos srcAnn = case GHC.ann srcAnn of
  GHC.EpAnn a _ _ | GHC.MovedAnchor dp <- GHC.anchor_op a -> Just dp
  _ -> Nothing
#endif

#if MIN_VERSION_ghc(9,10,0)
type SrcAnn a = GHC.EpAnn a
#else
type SrcAnn a = GHC.SrcAnn a
#endif

#if MIN_VERSION_ghc(9,10,0)
addCommentsToSrcAnn :: SrcAnn ann -> GHC.EpAnnComments -> SrcAnn ann
addCommentsToSrcAnn (GHC.EpAnn e a cs) cs' = GHC.EpAnn e a (cs <> cs')
#else
addCommentsToSrcAnn :: Monoid ann => SrcAnn ann -> GHC.EpAnnComments -> SrcAnn ann
addCommentsToSrcAnn = GHC.addCommentsToSrcAnn
#endif

#if MIN_VERSION_ghc(9,10,0)
type OpAppAnn = [GHC.AddEpAnn]
#else
type OpAppAnn = GHC.EpAnn [GHC.AddEpAnn]
#endif

#if MIN_VERSION_ghc(9,10,0)
mkAnchor :: AnnSpan -> GHC.DeltaPos -> GHC.NoCommentsLocation
mkAnchor _ dp = GHC.EpaDelta dp GHC.NoComments
#else
mkAnchor :: AnnSpan -> GHC.DeltaPos -> GHC.Anchor
mkAnchor r = GHC.Anchor r . GHC.MovedAnchor
#endif

lEpaCommentRealSrcSpan :: GHC.LEpaComment -> Maybe GHC.RealSrcSpan
#if MIN_VERSION_ghc(9,10,0)
lEpaCommentRealSrcSpan (GHC.L l _) = case l of
  GHC.EpaSpan (GHC.RealSrcSpan rss _) -> Just rss
  _ -> Nothing
#else
lEpaCommentRealSrcSpan (GHC.L (Anchor rss _) _) = Just rss
#endif

#if MIN_VERSION_ghc(9,10,0)
-- Copied from ghc-9.8.2
instance Semigroup GHC.AnnList where
  (GHC.AnnList a1 o1 c1 r1 t1) <> (GHC.AnnList a2 o2 c2 r2 t2)
    = GHC.AnnList (a1 <> a2) (c o1 o2) (c c1 c2) (r1 <> r2) (t1 <> t2)
    where
      -- Left biased combination for the open and close annotations
      c Nothing x = x
      c x Nothing = x
      c f _       = f
#endif

#if MIN_VERSION_ghc(9,10,0)
-- Copied from ghc-9.8.2
instance Monoid GHC.AnnListItem where
  mempty = GHC.AnnListItem []
#endif

mkErr :: DynFlags -> SrcSpan -> String -> Errors
#if MIN_VERSION_ghc(9,4,0)
mkErr _df l s =
  mkMessages $
    unitBag (mkPlainErrorMsgEnvelope l (ghcUnknownMessage $ mkDecoratedError [] [text s]))
#else
mkErr _df l s = unitBag (mkPlainMsgEnvelope l (text s))
#endif

parseModuleName :: SrcSpan -> Parser (LocatedA GHC.ModuleName)
parseModuleName ss _ _ s =
  let newMN =  GHC.L (GHC.noAnnSrcSpan ss) (GHC.mkModuleName s)
  in pure newMN

type DoGenReplacement an ast a =
  (Data ast, Data a) =>
  a ->
  (LocatedAn an ast -> Bool) ->
  LocatedAn an ast ->
  LocatedAn an ast ->
  StateT Bool IO (LocatedAn an ast)

type ReplaceWorker a mod =
  (Data a, Data mod) =>
  mod ->
  Parser (GHC.LocatedA a) ->
  Int ->
  Refactoring SrcSpan ->
  IO mod

#if MIN_VERSION_ghc_exactprint(1,10,0)
transferEntryDP ::
  forall t1 t2 a b.
  (Typeable t1, Typeable t2) =>
  LocatedAn t1 a ->
  LocatedAn t2 b ->
  LocatedAn t2 b
transferEntryDP = EP.transferEntryDP
#else
transferEntryDP ::
  forall t1 t2 a b.
  (Typeable t1, Typeable t2, Monoid t2) =>
  LocatedAn t1 a ->
  LocatedAn t2 b ->
  LocatedAn t2 b
transferEntryDP a b = let (c, _, _) = EP.runTransform (EP.transferEntryDP a b) in c
#endif

transferEntryDP' :: GHC.LHsDecl GHC.GhcPs -> GHC.LHsDecl GHC.GhcPs -> GHC.LHsDecl GHC.GhcPs
#if MIN_VERSION_ghc_exactprint(1,10,0)
transferEntryDP' = EP.transferEntryDP'
#else
transferEntryDP' a b = let (c, _, _) = EP.runTransform (EP.transferEntryDP' a b) in c
#endif

refactOptions :: EP.EPOptions Identity String
#if MIN_VERSION_ghc_exactprint(1,10,0)
refactOptions = EP.stringOptions
#else
refactOptions = EP.stringOptions {EP.epRigidity = EP.RigidLayout}
#endif

makeDeltaAst :: EP.ExactPrint ast => ast -> ast
#if MIN_VERSION_ghc(9,10,0)
makeDeltaAst = id
#else
makeDeltaAst = EP.makeDeltaAst
#endif
