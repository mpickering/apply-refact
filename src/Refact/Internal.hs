{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Refact.Internal
  ( apply
  , runRefactoring
  , addExtensionsToFlags
  , parseModuleWithArgs
  , parseExtensions

  -- * Support for runPipe in the main process
  , Verbosity(..)
  , rigidLayout
  , refactOptions
  , type Errors
  , onError
  , mkErr
  )  where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.State
import Data.Char (isAlphaNum)
import Data.Data
import Data.Foldable (foldlM, for_)
import Data.Functor.Identity (Identity(..))
import Data.Generics (everywhereM, extM, listify, mkM, mkQ, something)
import Data.Generics.Uniplate.Data (transformBi, transformBiM, universeBi)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.List.Extra
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra
import DynFlags hiding (initDynFlags)
import FastString (unpackFS)
import HeaderInfo (getOptions)
import HscTypes (handleSourceError)
import GHC.IO.Exception (IOErrorType(..))
import GHC.LanguageExtensions.Type (Extension(..))
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Annotate
import Language.Haskell.GHC.ExactPrint.Delta
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Print
import Language.Haskell.GHC.ExactPrint.Types hiding (GhcPs, GhcTc, GhcRn)
import Language.Haskell.GHC.ExactPrint.Utils
import Panic (handleGhcException)
import StringBuffer (stringToStringBuffer)
import System.IO.Error (mkIOError)
import System.IO.Extra

import Debug.Trace

#if __GLASGOW_HASKELL__ >= 810
import GHC.Hs.Expr as GHC hiding (Stmt)
import GHC.Hs.ImpExp
import GHC.Hs hiding (Pat, Stmt)
import ErrUtils
import Bag
#else
import HsExpr as GHC hiding (Stmt)
import HsImpExp
import HsSyn hiding (Pat, Stmt, noExt)
#endif

import Outputable hiding ((<>))
import SrcLoc hiding (spans)
import qualified GHC hiding (parseModule)
import qualified Name as GHC
import qualified RdrName as GHC

import Refact.Types hiding (SrcSpan)
import qualified Refact.Types as R
import Refact.Utils (Stmt, Pat, Name, Decl, M, Module, Expr, Type, FunBind, AnnKeyMap
                    , modifyAnnKey, replaceAnnKey, Import, toGhcSrcSpan, toGhcSrcSpan', setSrcSpanFile)

#if __GLASGOW_HASKELL__ >= 810
type Errors = ErrorMessages
onError :: String -> Errors -> a
onError s = pprPanic s . vcat . pprErrMsgBagWithLoc
#else
type Errors = (SrcSpan, String)
onError :: String -> Errors -> a
onError _ = error . show
#endif

#if __GLASGOW_HASKELL__ <= 806
composeSrcSpan :: a -> a
composeSrcSpan = id

decomposeSrcSpan :: a -> a
decomposeSrcSpan = id

type SrcSpanLess a = a
#endif

-- library access to perform the substitutions

refactOptions :: PrintOptions Identity String
refactOptions = stringOptions { epRigidity = RigidLayout }

rigidLayout :: DeltaOptions
rigidLayout = deltaOptions RigidLayout

-- | Apply a set of refactorings as supplied by hlint
apply
  :: Maybe (Int, Int)
  -> Bool
  -> [(String, [Refactoring R.SrcSpan])]
  -> Maybe FilePath
  -> Verbosity
  -> Anns
  -> Module
  -> IO String
apply mpos step inp mbfile verb as0 m0 = do
  toGhcSS <-
    maybe
      ( case getLoc m0 of
          UnhelpfulSpan s -> fail $ "Module has UnhelpfulSpan: " ++ unpackFS s
          RealSrcSpan s -> pure $ toGhcSrcSpan' (srcSpanFile s)
      )
      (pure . toGhcSrcSpan)
      mbfile
  let allRefacts :: [((String, [Refactoring SrcSpan]), R.SrcSpan)]
      allRefacts =
        sortBy cmpSrcSpan
        . map (first . second . map . fmap $ toGhcSS)
        . mapMaybe (sequenceA . (id &&& aggregateSrcSpans . map pos . snd))
        . filter (maybe (const True) (\p -> any ((`spans` p) . pos) . snd) mpos)
        $ inp

      cmpSrcSpan (_, s1) (_, s2) =
        comparing startLine s1 s2 <> -- s1 first if it starts on earlier line
        comparing startCol s1 s2 <>  --             or on earlier column
        comparing endLine s2 s1 <>   -- they start in same place, s2 comes
        comparing endCol s2 s1       -- first if it ends later
        -- else, completely same span, so s1 will be first

  when (verb >= Normal) . traceM $
    "Applying " ++ (show . sum . map (length . snd . fst) $ allRefacts) ++ " hints"
  when (verb == Loud) . traceM $ show (map fst allRefacts)

  (as, m) <- if step
               then fromMaybe (as0, m0) <$> runMaybeT (refactoringLoop as0 m0 allRefacts)
               else evalStateT (runRefactorings verb as0 m0 (first snd <$> allRefacts)) 0
  pure . runIdentity $ exactPrintWithOptions refactOptions m as

spans :: R.SrcSpan -> (Int, Int) -> Bool
spans R.SrcSpan{..} loc = (startLine, startCol) <= loc && loc <= (endLine, endCol)

aggregateSrcSpans :: [R.SrcSpan] -> Maybe R.SrcSpan
aggregateSrcSpans = \case
  [] -> Nothing
  rs -> Just (foldr1 alg rs)
  where
    alg (R.SrcSpan sl1 sc1 el1 ec1) (R.SrcSpan sl2 sc2 el2 ec2) =
      let (sl, sc) = case compare sl1 sl2 of
                      LT -> (sl1, sc1)
                      EQ -> (sl1, min sc1 sc2)
                      GT -> (sl2, sc2)
          (el, ec) = case compare el1 el2 of
                      LT -> (el2, ec2)
                      EQ -> (el2, max ec1 ec2)
                      GT -> (el1, ec1)
       in R.SrcSpan sl sc el ec

runRefactorings
  :: Verbosity
  -> Anns
  -> Module
  -> [([Refactoring SrcSpan], R.SrcSpan)]
  -> StateT Int IO (Anns, Module)
runRefactorings verb as0 m0 ((rs, ss) : rest) = do
  runRefactorings' verb as0 m0 rs >>= \case
    Nothing -> runRefactorings verb as0 m0 rest
    Just (as, m) -> do
      let (overlaps, rest') = span (overlap ss . snd) rest
      when (verb >= Normal) . for_ overlaps $ \(rs', _) ->
        traceM $ "Ignoring " ++ show rs' ++ " due to overlap."
      runRefactorings verb as m rest'
runRefactorings _ as m [] = pure (as, m)

runRefactorings'
  :: Verbosity
  -> Anns
  -> Module
  -> [Refactoring SrcSpan]
  -> StateT Int IO (Maybe (Anns, Module))
runRefactorings' verb as0 m0 rs = do
  seed <- get
  (as, m, keyMap) <- foldlM (uncurry3 runRefactoring) (as0, m0, Map.empty) rs
  if droppedComments as m keyMap
    then
      do
        put seed
        when (verb >= Normal) . traceM $
          "Ignoring " ++ show rs ++ " since applying them would cause comments to be dropped."
        pure Nothing
    else pure $ Just (as, m)

overlap :: R.SrcSpan -> R.SrcSpan -> Bool
overlap s1 s2 =
  -- We know s1 always starts <= s2, due to our sort
  case compare (startLine s2) (endLine s1) of
    LT -> True
    EQ -> startCol s2 <= endCol s1
    GT -> False

data LoopOption = LoopOption
  { desc    :: String
  , perform :: MaybeT IO (Anns, Module)
  }

refactoringLoop
  :: Anns
  -> Module
  -> [((String, [Refactoring SrcSpan]), R.SrcSpan)]
  -> MaybeT IO (Anns, Module)
refactoringLoop as m [] = pure (as, m)
refactoringLoop as m (((_, []), _): rs) = refactoringLoop as m rs
refactoringLoop as0 m0 hints@(((hintDesc, rs), ss): rss) = do
    res <- liftIO . flip evalStateT 0 $ runRefactorings' Silent as0 m0 rs
    let yAction = case res of
          Just (as, m) -> do
            exactPrint m as `seq` pure ()
            refactoringLoop as m $ dropWhile (overlap ss . snd) rss
          Nothing -> do
            liftIO $ putStrLn "Hint skipped since applying it would cause comments to be dropped"
            refactoringLoop as0 m0 rss
        opts =
          [ ("y", LoopOption "Apply current hint" yAction)
          , ("n", LoopOption "Don't apply the current hint" (refactoringLoop as0 m0 rss))
          , ("q", LoopOption "Apply no further hints" (pure (as0, m0)))
          , ("d", LoopOption "Discard previous changes" mzero )
          , ("v", LoopOption "View current file" (liftIO (putStrLn (exactPrint m0 as0))
                                                  >> refactoringLoop as0 m0 hints))
          , ("?", LoopOption "Show this help menu" loopHelp)]
        loopHelp = do
          liftIO . putStrLn . unlines . map mkLine $ opts
          refactoringLoop as0 m0 hints
        mkLine (c, opt) = c ++ " - " ++ desc opt
    inp <- liftIO $ do
      putStrLn hintDesc
      putStrLn $ "Apply hint [" ++ intercalate ", " (map fst opts) ++ "]"
      -- In case that the input also comes from stdin
      withFile "/dev/tty" ReadMode hGetLine
    maybe loopHelp perform (lookup inp opts)

data Verbosity = Silent | Normal | Loud deriving (Eq, Show, Ord)

-- ---------------------------------------------------------------------

-- Perform the substitutions

-- | Peform a @Refactoring@.
runRefactoring
  :: Data a
  => Anns
  -> a
  -> AnnKeyMap
  -> Refactoring SrcSpan
  -> StateT Int IO (Anns, a, AnnKeyMap)
runRefactoring as m keyMap = \case
  r@Replace{} -> do
    seed <- get <* modify (+1)
    liftIO $ case rtype r of
      Expr -> replaceWorker as m keyMap parseExpr seed r
      Decl -> replaceWorker as m keyMap parseDecl seed r
      Type -> replaceWorker as m keyMap parseType seed r
      Pattern -> replaceWorker as m keyMap parsePattern seed r
      Stmt -> replaceWorker as m keyMap parseStmt seed r
      Bind -> replaceWorker as m keyMap parseBind seed r
      R.Match ->  replaceWorker as m keyMap parseMatch seed r
      ModuleName -> replaceWorker as m keyMap (parseModuleName (pos r)) seed r
      Import -> replaceWorker as m keyMap parseImport seed r

  ModifyComment{..} -> pure (Map.map go as, m, keyMap)
    where
      go a@Ann{ annPriorComments, annsDP } =
        a { annsDP = map changeComment annsDP
          , annPriorComments = map (first change) annPriorComments }
      changeComment (AnnComment d, dp) = (AnnComment (change d), dp)
      changeComment e = e
      change old@Comment{..}= if ss2pos commentIdentifier == ss2pos pos
                                          then old { commentContents = newComment}
                                          else old
  Delete{rtype, pos} -> pure (as, f m, keyMap)
    where
      f = case rtype of
        Stmt -> doDeleteStmt ((/= pos) . getLoc)
        Import -> doDeleteImport ((/= pos) . getLoc)
        _ -> id

  InsertComment{..} -> do
    exprkey <- mkAnnKey <$> findOrError @(HsDecl GhcPs) m pos
    pure (insertComment exprkey newComment as, m, keyMap)

  RemoveAsKeyword{..} -> pure (as, removeAsKeyword m, keyMap)
    where
      removeAsKeyword = transformBi go
      go :: LImportDecl GHC.GhcPs -> LImportDecl GHC.GhcPs
      go imp@(GHC.L l i)
        | l == pos = GHC.L l (i { ideclAs = Nothing })
        | otherwise =  imp

droppedComments :: Anns -> Module -> AnnKeyMap -> Bool
droppedComments as m keyMap = any (all (`Set.notMember` allSpans)) spanssWithComments
  where
    spanssWithComments =
      map (\(key, _) -> map keySpan $ key : Map.findWithDefault [] key keyMap)
      . filter (\(_, v) -> notNull (annPriorComments v) || notNull (annFollowingComments v))
      $ Map.toList as

    keySpan (AnnKey ss _) = ss

    allSpans :: Set SrcSpan
    allSpans = Set.fromList (universeBi m)

-- Specialised parsers
mkErr :: GHC.DynFlags -> SrcSpan -> String -> Errors
#if __GLASGOW_HASKELL__ >= 810
mkErr df l s = unitBag (mkPlainErrMsg df l (text s))
#else
mkErr = const (,)
#endif

parseModuleName :: SrcSpan -> Parser (GHC.Located GHC.ModuleName)
parseModuleName ss _ _ s =
  let newMN =  GHC.L ss (GHC.mkModuleName s)
      newAnns = relativiseApiAnns newMN (Map.empty, Map.empty)
  in pure (newAnns, newMN)
parseBind :: Parser (GHC.LHsBind GHC.GhcPs)
parseBind dyn fname s =
  case parseDecl dyn fname s of
    -- Safe as we add no annotations to the ValD
    Right (as, GHC.L l (GHC.ValD _ b)) -> Right (as, GHC.L l b)
    Right (_, GHC.L l _) -> Left (mkErr dyn l "Not a HsBind")
    Left e -> Left e
parseMatch :: Parser (GHC.LMatch GHC.GhcPs (GHC.LHsExpr GHC.GhcPs))
parseMatch dyn fname s =
  case parseBind dyn fname s of
    Right (as, GHC.L l GHC.FunBind{fun_matches}) ->
      case unLoc (GHC.mg_alts fun_matches) of
           [x] -> Right (as, x)
           _   -> Left (mkErr dyn l "Not a single match")
    Right (_, GHC.L l _) -> Left (mkErr dyn l "Not a funbind")
    Left e -> Left e

-- Substitute variables into templates
-- Finds places in the templates where we need to insert variables.

substTransform :: (Data a, Data b) => b -> [(String, SrcSpan)] -> a -> M a
substTransform m ss = everywhereM (mkM (typeSub m ss)
                                    `extM` identSub m ss
                                    `extM` patSub m ss
                                    `extM` stmtSub m ss
                                    `extM` exprSub m ss
                                    )

stmtSub :: Data a => a -> [(String, SrcSpan)] -> Stmt -> M Stmt
stmtSub m subs old@(GHC.L _ (BodyStmt _ (GHC.L _ (HsVar _ (L _ name))) _ _) ) =
  resolveRdrName m (findOrError m) old subs name
stmtSub _ _ e = pure e

patSub :: Data a => a -> [(String, SrcSpan)] -> Pat -> M Pat
patSub m subs old@(GHC.L _ (VarPat _ (L _ name))) =
  resolveRdrName m (findOrError m) old subs name
patSub _ _ e = pure e

typeSub :: Data a => a -> [(String, SrcSpan)] -> Type -> M Type
typeSub m subs old@(GHC.L _ (HsTyVar _ _ (L _ name))) =
  resolveRdrName m (findOrError m) old subs name
typeSub _ _ e = pure e

exprSub :: Data a => a -> [(String, SrcSpan)] -> Expr -> M Expr
exprSub m subs old@(GHC.L _ (HsVar _ (L _ name))) =
  resolveRdrName m (findOrError m) old subs name
exprSub _ _ e = pure e

-- Used for Monad10, Monad11 tests.
-- The issue being that in one case the information is attached to a VarPat
-- but we need to move the annotations onto the actual name
--
-- This looks convoluted but we can't match directly on a located name as
-- it is not specific enough. Instead we match on some bigger context which
-- is contains the located name we want to replace.
identSub :: Data a => a -> [(String, SrcSpan)] -> FunBind -> M FunBind
identSub m subs old@(GHC.FunRhs (GHC.L _ name) _ _) =
  resolveRdrName' subst (findOrError m) old subs name
  where
    subst :: FunBind -> Name -> M FunBind
    subst (GHC.FunRhs n b s) new = do
      let fakeExpr :: Located (GHC.Pat GhcPs)
          fakeExpr = GHC.L (getLoc new) (GHC.VarPat noExt new)
      -- Low level version as we need to combine the annotation information
      -- from the template RdrName and the original VarPat.
      modify . first $
        replaceAnnKey (mkAnnKey n) (mkAnnKey fakeExpr) (mkAnnKey new) (mkAnnKey fakeExpr)
      pure $ GHC.FunRhs new b s
    subst o _ = pure o
identSub _ _ e = pure e


-- g is usually modifyAnnKey
-- f is usually a function which checks the locations are equal
resolveRdrName' :: (a -> b -> M a)  -- How to combine the value to insert and the replaced value
               -> (SrcSpan -> M b)    -- How to find the new value, when given the location it is in
               -> a                 -- The old thing which we are going to possibly replace
               -> [(String, SrcSpan)] -- Substs
               -> GHC.RdrName       -- The name of the position in the template
                                    --we are replacing into
               -> M a
resolveRdrName' g f old subs name =
  case name of
    -- Todo: this should replace anns as well?
    GHC.Unqual (GHC.occNameString . GHC.occName -> oname)
      | Just ss <- lookup oname subs -> f ss >>= g old
    _ -> pure old

resolveRdrName :: (Data old, Data a)
               => a
               -> (SrcSpan -> M (Located old))
               -> Located old
               -> [(String, SrcSpan)]
               -> GHC.RdrName
               -> M (Located old)
resolveRdrName m = resolveRdrName' (modifyAnnKey m)

insertComment :: AnnKey -> String
              -> Map.Map AnnKey Annotation
              -> Map.Map AnnKey Annotation
insertComment k s as =
  let comment = Comment s GHC.noSrcSpan Nothing in
  Map.adjust (\a@Ann{..} -> a { annPriorComments = annPriorComments ++ [(comment, DP (1,0))]
                          , annEntryDelta = DP (1,0) }) k as


-- Substitute the template into the original AST.
#if __GLASGOW_HASKELL__ <= 806
doGenReplacement
  :: forall ast a. (Data ast, Data a)
  => a
  -> (GHC.Located ast -> Bool)
  -> GHC.Located ast
  -> GHC.Located ast
  -> StateT ((Anns, AnnKeyMap), Bool) IO (GHC.Located ast)
#else
doGenReplacement
  :: forall ast a. (Data (SrcSpanLess ast), HasSrcSpan ast, Data a)
  => a
  -> (ast -> Bool)
  -> ast
  -> ast
  -> StateT ((Anns, AnnKeyMap), Bool) IO ast
#endif
doGenReplacement m p new old
  | p old = do
      (anns, keyMap) <- gets fst
      let n = decomposeSrcSpan new
          o = decomposeSrcSpan old
      (newAnns, newKeyMap) <- liftIO $ execStateT (modifyAnnKey m o n) (anns, keyMap)
      put ((newAnns, newKeyMap), True)
      pure new
  -- If "f a = body where local" doesn't satisfy the predicate, but "f a = body" does,
  -- run the replacement on "f a = body", and add "local" back afterwards.
  -- This is useful for hints like "Eta reduce" and "Redundant where".
  | Just Refl <- eqT @(SrcSpanLess ast) @(HsDecl GHC.GhcPs)
  , L _ (ValD xvald newBind@FunBind{}) <- decomposeSrcSpan new
  , Just (oldNoLocal, oldLocal) <- stripLocalBind (decomposeSrcSpan old)
  , newLoc@(RealSrcSpan newLocReal) <- getLoc new
  , p (composeSrcSpan oldNoLocal) = do
      (anns, keyMap) <- gets fst
      let n = decomposeSrcSpan new
          o = decomposeSrcSpan old
      (intAnns, newKeyMap) <- liftIO $ execStateT (modifyAnnKey m o n) (anns, keyMap)
      let newFile = srcSpanFile newLocReal
          newLocal = transformBi (setSrcSpanFile newFile) oldLocal
          newLocalLoc = getLoc newLocal
          ensureLoc = combineSrcSpans newLocalLoc
          newMG = fun_matches newBind
          L locMG [L locMatch newMatch] = mg_alts newMG
          newGRHSs = m_grhss newMatch
          finalLoc = ensureLoc newLoc
          newWithLocalBinds = setLocalBind newLocal xvald newBind finalLoc
                                           newMG (ensureLoc locMG) newMatch (ensureLoc locMatch) newGRHSs

          -- Ensure the new Anns properly reflects the local binds we added back.
          addLocalBindsToAnns = addAnnWhere
                              . Map.fromList
                              . map (first (expandTemplateLoc . updateFile . expandGRHSLoc))
                              . Map.toList
            where
              addAnnWhere :: Anns -> Anns
              addAnnWhere oldAnns =
                let oldAnns' = Map.toList oldAnns
                    po = \case
                      (AnnKey loc@(RealSrcSpan r) con, _) ->
                        loc == getLoc old && con == CN "Match" && srcSpanFile r /= newFile
                      _ -> False
                    pn = \case
                      (AnnKey loc@(RealSrcSpan r) con, _) ->
                        loc == finalLoc && con == CN "Match" && srcSpanFile r == newFile
                      _ -> False
                in fromMaybe oldAnns $ do
                      oldAnn <- snd <$> find po oldAnns'
                      annWhere <- find ((== G GHC.AnnWhere) . fst) (annsDP oldAnn)
                      let newSortKey = fmap (setSrcSpanFile newFile) <$> annSortKey oldAnn
                      newKey <- fst <$> find pn oldAnns'
                      pure $ Map.adjust
                        (\ann -> ann {annsDP = annsDP ann ++ [annWhere], annSortKey = newSortKey})
                        newKey
                        oldAnns

              -- Expand the SrcSpan of the "GRHS" entry in the new file to include the local binds
              expandGRHSLoc = \case
                AnnKey loc@(RealSrcSpan r) con
                  | con == CN "GRHS", srcSpanFile r == newFile -> AnnKey (ensureLoc loc) con
                other -> other

              -- If an Anns entry corresponds to the local binds, update its file to point to the new file.
              updateFile = \case
                AnnKey loc con
                  | loc `isSubspanOf` getLoc oldLocal -> AnnKey (setSrcSpanFile newFile loc) con
                other -> other

              -- For each SrcSpan in the new file that is the entire newLoc, set it to finalLoc
              expandTemplateLoc = \case
                AnnKey loc con
                  | loc == newLoc -> AnnKey finalLoc con
                other -> other

          newAnns = addLocalBindsToAnns intAnns
      put ((newAnns, newKeyMap), True)
      pure $ composeSrcSpan newWithLocalBinds
  | otherwise = pure old

-- | If the input is a FunBind with a single match, e.g., "foo a = body where x = y"
-- return "Just (foo a = body, x = y)". Otherwise return Nothing.
stripLocalBind
  :: LHsDecl GHC.GhcPs
  -> Maybe (LHsDecl GHC.GhcPs, LHsLocalBinds GHC.GhcPs)
stripLocalBind = \case
  L _ (ValD xvald origBind@FunBind{})
    | let origMG = fun_matches origBind
    , L locMG [L locMatch origMatch] <- mg_alts origMG
    , let origGRHSs = m_grhss origMatch
    , [L _ (GRHS _ _ (L loc2 _))] <- grhssGRHSs origGRHSs ->
      let loc1 = getLoc (fun_id origBind)
          newLoc = combineSrcSpans loc1 loc2
          withoutLocalBinds = setLocalBind (noLoc (EmptyLocalBinds noExt)) xvald origBind newLoc origMG locMG
                                           origMatch locMatch origGRHSs
       in Just (withoutLocalBinds, grhssLocalBinds origGRHSs)
  _ -> Nothing

-- | Set the local binds in a HsBind.
setLocalBind
  :: LHsLocalBinds GHC.GhcPs
  -> XValD GhcPs
  -> HsBind GhcPs
  -> SrcSpan
  -> MatchGroup GhcPs (LHsExpr GhcPs)
  -> SrcSpan
  -> Match GhcPs (LHsExpr GhcPs)
  -> SrcSpan
  -> GRHSs GhcPs (LHsExpr GhcPs)
  -> LHsDecl GhcPs
setLocalBind newLocalBinds xvald origBind newLoc origMG locMG origMatch locMatch origGRHSs =
    L newLoc (ValD xvald newBind)
  where
    newGRHSs = origGRHSs{grhssLocalBinds = newLocalBinds}
    newMatch = origMatch{m_grhss = newGRHSs}
    newMG = origMG{mg_alts = L locMG [L locMatch newMatch]}
    newBind = origBind{fun_matches = newMG}

#if __GLASGOW_HASKELL__ <= 806
replaceWorker :: (Annotate a, Data mod)
              => Anns
              -> mod
              -> AnnKeyMap
              -> Parser (GHC.Located a)
              -> Int
              -> Refactoring SrcSpan
              -> IO (Anns, mod, AnnKeyMap)
#else
replaceWorker :: (Annotate a, HasSrcSpan a, Data mod, Data (SrcSpanLess a))
              => Anns
              -> mod
              -> AnnKeyMap
              -> Parser a
              -> Int
              -> Refactoring SrcSpan
              -> IO (Anns, mod, AnnKeyMap)
#endif
replaceWorker as m keyMap parser seed Replace{..} = do
  let replExprLocation = pos
      uniqueName = "template" ++ show seed

  (relat, template) <- withDynFlags (\d -> parser d uniqueName orig) >>=
    either (onError "replaceWorked") pure

  (newExpr, (newAnns, newKeyMap)) <-
    runStateT
      (substTransform m subts template)
      (mergeAnns as relat, keyMap)
  let lst = listToMaybe . reverse . GHC.occNameString . GHC.rdrNameOcc
      adjacent (srcSpanEnd -> RealSrcLoc loc1) (srcSpanStart -> RealSrcLoc loc2) = loc1 == loc2
      adjacent _ _ = False

      -- Ensure that there is a space between two alphanumeric names, otherwise
      -- 'y = f(x)' would be refactored into 'y = fx'.
      ensureSpace :: Anns -> Anns
      ensureSpace = fromMaybe id $ do
        (L _ (HsVar _ (L _ newName))) :: LHsExpr GhcPs <- cast newExpr
        hd <- listToMaybe $ case newName of
          GHC.Unqual occName -> GHC.occNameString occName
          GHC.Qual moduleName _ -> GHC.moduleNameString moduleName
          GHC.Orig modu _ -> GHC.moduleNameString (GHC.moduleName modu)
          GHC.Exact name -> GHC.occNameString (GHC.nameOccName name)
        guard $ isAlphaNum hd
        let prev :: [LHsExpr GhcPs] =
              listify
                (\case
                   (L loc (HsVar _ (L _ rdr))) -> maybe False isAlphaNum (lst rdr) && adjacent loc pos
                   _ -> False
                )
                m
        guard . not . null $ prev
        pure . flip Map.adjust (mkAnnKey newExpr) $ \ann ->
          if annEntryDelta ann == DP (0, 0)
            then ann { annEntryDelta = DP (0, 1) }
            else ann

      replacementPred (GHC.L l _) = l == replExprLocation
      transformation = transformBiM (doGenReplacement m (replacementPred . decomposeSrcSpan) newExpr)
  runStateT (transformation m) ((newAnns, newKeyMap), False) >>= \case
    (finalM, ((finalAs, finalKeyMap), True)) -> pure (ensureSpace finalAs, finalM, finalKeyMap)
    -- Failed to find a replacment so don't make any changes
    _ -> pure (as, m, keyMap)
replaceWorker as m keyMap _ _ _ = pure (as, m, keyMap)

data NotFound = NotFound
  { nfExpected :: String
  , nfActual :: Maybe String
  , nfLoc :: SrcSpan
  }

renderNotFound :: NotFound -> String
renderNotFound NotFound{..} =
  "Expected type not found at the location specified in the refact file.\n"
  ++ "  Expected type: " ++ nfExpected ++ "\n"
  ++ maybe "" (\actual -> "  Actual type: " ++ actual ++ "\n") nfActual
  ++ "  Location: " ++ showSDocUnsafe (ppr nfLoc)

-- Find a given type with a given SrcSpan
findInModule :: forall a modu. (Data a, Data modu) => modu -> SrcSpan -> Either NotFound (GHC.Located a)
findInModule m ss = case doTrans m of
  Just a -> Right a
  Nothing ->
    let expected = show (typeRep (Proxy @a))
        actual = listToMaybe $ catMaybes
          [ showType (doTrans m :: Maybe Expr)
          , showType (doTrans m :: Maybe Type)
          , showType (doTrans m :: Maybe Decl)
          , showType (doTrans m :: Maybe Pat)
          , showType (doTrans m :: Maybe Name)
          ]
     in Left $ NotFound expected actual ss
  where
    doTrans :: forall b. Data b => modu -> Maybe (GHC.Located b)
    doTrans = something (mkQ Nothing (findLargestExpression ss))

    showType :: forall b. Typeable b => Maybe (GHC.Located b) -> Maybe String
    showType = fmap $ \_ -> show (typeRep (Proxy @b))

findLargestExpression :: SrcSpan -> GHC.Located a -> Maybe (GHC.Located a)
findLargestExpression ss e@(GHC.L l _)
  | l == ss = Just e
  | otherwise = Nothing

findOrError
  :: forall a modu m. (Data a, Data modu, MonadIO m)
  => modu -> SrcSpan -> m (GHC.Located a)
findOrError m = either f pure . findInModule m
  where
    f nf = liftIO . throwIO $ mkIOError InappropriateType (renderNotFound nf) Nothing Nothing

-- Deletion from a list

doDeleteStmt :: Data a => (Stmt -> Bool) -> a -> a
doDeleteStmt = transformBi . filter

doDeleteImport :: Data a => (Import -> Bool) -> a -> a
doDeleteImport = transformBi . filter

{-
-- Renaming

doRename :: [(String, String)] -> Module -> Module
doRename ss = everywhere (mkT rename)
  where
    rename :: GHC.OccName -> GHC.OccName
    rename v = GHC.mkOccName n s'
      where
          (s, n) = (GHC.occNameString v, GHC.occNameSpace v)
          s' = fromMaybe s (lookup s ss)
-}

addExtensionsToFlags
  :: [Extension] -> [Extension] -> FilePath -> DynFlags
  -> IO (Either String DynFlags)
addExtensionsToFlags es ds fp flags = catchErrors $ do
    (stringToStringBuffer -> buf) <- readFileUTF8' fp
    let opts = getOptions flags buf fp
        withExts = flip (foldl' xopt_unset) ds
                      . flip (foldl' xopt_set) es
                      $ flags
    (withPragmas, _, _) <- parseDynamicFilePragma withExts opts
    pure . Right $ withPragmas `gopt_set` Opt_KeepRawTokenStream
  where
    catchErrors = handleGhcException (pure . Left . show)
                . handleSourceError (pure . Left . show)

parseModuleWithArgs
  :: ([Extension], [Extension])
  -> FilePath
  -> IO (Either Errors (Anns, GHC.ParsedSource))
parseModuleWithArgs (es, ds) fp = ghcWrapper $ do
  initFlags <- initDynFlags fp
  eflags <- liftIO $ addExtensionsToFlags es ds fp initFlags
  case eflags of
    -- TODO: report error properly.
    Left err -> pure . Left $ mkErr initFlags (UnhelpfulSpan mempty) err
    Right flags -> do
      _ <- GHC.setSessionDynFlags flags
      res <- parseModuleApiAnnsWithCppInternal defaultCppOptions flags fp
      pure $ postParseTransform res rigidLayout


-- | Parse the input into (enabled extensions, disabled extensions, invalid input).
-- Implied extensions are automatically added. For example, @FunctionalDependencies@
-- implies @MultiParamTypeClasses@, and @RebindableSyntax@ implies @NoImplicitPrelude@.
--
-- The input is processed from left to right. An extension (e.g., @StarIsType@)
-- may be overridden later (e.g., by @NoStarIsType@).
--
-- Extensions that appear earlier in the input will appear later in the output.
-- Implied extensions appear in the end. If an extension occurs multiple times in the input,
-- the last one is used.
--
-- >>> parseExtensions ["GADTs", "RebindableSyntax", "StarIsType", "GADTs", "InvalidExtension", "NoStarIsType"]
-- ([GADTs, RebindableSyntax, GADTSyntax, MonoLocalBinds], [StarIsType, ImplicitPrelude], ["InvalidExtension"])
parseExtensions :: [String] -> ([Extension], [Extension], [String])
parseExtensions = addImplied . foldl' f mempty
  where
    f :: ([Extension], [Extension], [String]) -> String -> ([Extension], [Extension], [String])
    f (ys, ns, is) ('N' : 'o' : s) | Just ext <- readExtension s =
      (delete ext ys, ext : delete ext ns, is)
    f (ys, ns, is) s | Just ext <- readExtension s =
      (ext : delete ext ys, delete ext ns, is)
    f (ys, ns, is) s = (ys, ns, s : is)

    addImplied :: ([Extension], [Extension], [String]) -> ([Extension], [Extension], [String])
    addImplied (ys, ns, is) = (ys ++ impliedOn, ns ++ impliedOff, is)
      where
        impliedOn = [b | ext <- ys, (a, True, b) <- impliedXFlags, a == ext]
        impliedOff = [b | ext <- ys, (a, False, b) <- impliedXFlags, a == ext]

readExtension :: String -> Maybe Extension
readExtension s = flagSpecFlag <$> find ((== s) . flagSpecName) xFlags

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
