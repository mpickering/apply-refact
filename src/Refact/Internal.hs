{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
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
import Data.IORef.Extra
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.List.Extra
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra
import Debug.Trace

import qualified GHC

import GHC.IO.Exception (IOErrorType(..))
import GHC.LanguageExtensions.Type (Extension (..))
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Delta
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Print
import Language.Haskell.GHC.ExactPrint.Types hiding (GhcPs, GhcTc, GhcRn)
import Language.Haskell.GHC.ExactPrint.Utils hiding (rs)
import System.IO.Error (mkIOError)
import System.IO.Extra
import System.IO.Unsafe (unsafePerformIO)

import Refact.Types hiding (SrcSpan)
import qualified Refact.Types as R
import Refact.Utils (Stmt, Pat, Name, Decl, M, Expr, Type, AnnKeyMap
                    , modifyAnnKey, replaceAnnKey, Import, toGhcSrcSpan, toGhcSrcSpan'
                    ,  getAnnSpan, foldAnnKey)

import Refact.Compat (Errors, onError, Module, FunBind, pattern RealSrcSpan', pattern RealSrcLoc',
  annSpanToSrcSpan, srcSpanToAnnSpan, setSrcSpanFile, setAnnSpanFile,
  composeSrcSpan, decomposeSrcSpan, occNameString, nameOccName, rdrNameOcc, DoGenReplacement, SrcSpanLess,
  occName, badAnnSpan, combineSrcSpans, parseDynamicFilePragma,
  gopt_set, handleGhcException, FlagSpec (..), xFlags, impliedXFlags,
  mkErr, parseModuleName,
  showSDocUnsafe, ppr, stringToStringBuffer, getOptions, xopt_set, xopt_unset,
  ReplaceWorker, RdrName (..))

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
      ( case GHC.getLoc m0 of
          GHC.UnhelpfulSpan s -> fail $ "Module has UnhelpfulSpan: " ++ show s
          RealSrcSpan' s ->
            pure $ toGhcSrcSpan' (GHC.srcSpanFile s)
      )
      (pure . toGhcSrcSpan)
      mbfile
  let allRefacts :: [((String, [Refactoring GHC.SrcSpan]), R.SrcSpan)]
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
  -> [([Refactoring GHC.SrcSpan], R.SrcSpan)]
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
  -> [Refactoring GHC.SrcSpan]
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
  -> [((String, [Refactoring GHC.SrcSpan]), R.SrcSpan)]
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
  -> Refactoring GHC.SrcSpan
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
      change old@Comment{..}= if ss2pos (annSpanToSrcSpan commentIdentifier) == ss2pos pos
                                          then old { commentContents = newComment}
                                          else old
  Delete{rtype, pos} -> pure (as, f m, keyMap)
    where
      annSpan = srcSpanToAnnSpan pos
      f = case rtype of
        Stmt -> doDeleteStmt ((/= annSpan) . getAnnSpan)
        Import -> doDeleteImport ((/= annSpan) . getAnnSpan)
        _ -> id

  InsertComment{..} -> do
    exprkey <- mkAnnKey <$> findOrError @(GHC.HsDecl GHC.GhcPs) m (srcSpanToAnnSpan pos)
    pure (insertComment exprkey newComment as, m, keyMap)

  RemoveAsKeyword{..} -> pure (as, removeAsKeyword m, keyMap)
    where
      removeAsKeyword = transformBi go
      go :: GHC.LImportDecl GHC.GhcPs -> GHC.LImportDecl GHC.GhcPs
      go imp@(GHC.L l i)
        | srcSpanToAnnSpan l == srcSpanToAnnSpan pos = GHC.L l (i { GHC.ideclAs = Nothing })
        | otherwise =  imp

droppedComments :: Anns -> Module -> AnnKeyMap -> Bool
droppedComments as m keyMap = any (all (`Set.notMember` allSpans)) spanssWithComments
  where
    spanssWithComments =
      map (\(key, _) -> map keySpan $ key : Map.findWithDefault [] key keyMap)
      . filter (\(_, v) -> notNull (annPriorComments v) || notNull (annFollowingComments v))
      $ Map.toList as

    keySpan (AnnKey ss _) = ss

    allSpans :: Set AnnSpan
    allSpans = Set.fromList . fmap srcSpanToAnnSpan $ universeBi m


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
      case GHC.unLoc (GHC.mg_alts fun_matches) of
           [x] -> Right (as, x)
           _   -> Left (mkErr dyn l "Not a single match")
    Right (_, GHC.L l _) -> Left (mkErr dyn l "Not a funbind")
    Left e -> Left e

-- Substitute variables into templates
-- Finds places in the templates where we need to insert variables.

substTransform :: (Data a, Data b) => b -> [(String, GHC.SrcSpan)] -> a -> M a
substTransform m ss = everywhereM (mkM (typeSub m ss)
                                    `extM` identSub m ss
                                    `extM` patSub m ss
                                    `extM` stmtSub m ss
                                    `extM` exprSub m ss
                                    )

stmtSub :: Data a => a -> [(String, GHC.SrcSpan)] -> Stmt -> M Stmt
stmtSub m subs old@(GHC.L _ (GHC.BodyStmt _ (GHC.L _ (GHC.HsVar _ (GHC.L _ name))) _ _) ) =
  resolveRdrName m (findOrError m) old subs name
stmtSub _ _ e = pure e

patSub :: Data a => a -> [(String, GHC.SrcSpan)] -> Pat -> M Pat
patSub m subs old@(GHC.L _ (GHC.VarPat _ (GHC.L _ name))) =
  resolveRdrName m (findOrError m) old subs name
patSub _ _ e = pure e

typeSub :: Data a => a -> [(String, GHC.SrcSpan)] -> Type -> M Type
typeSub m subs old@(GHC.L _ (GHC.HsTyVar _ _ (GHC.L _ name))) =
  resolveRdrName m (findOrError m) old subs name
typeSub _ _ e = pure e

exprSub :: Data a => a -> [(String, GHC.SrcSpan)] -> Expr -> M Expr
exprSub m subs old@(GHC.L _ (GHC.HsVar _ (GHC.L _ name))) =
  resolveRdrName m (findOrError m) old subs name
exprSub _ _ e = pure e

-- Used for Monad10, Monad11 tests.
-- The issue being that in one case the information is attached to a VarPat
-- but we need to move the annotations onto the actual name
--
-- This looks convoluted but we can't match directly on a located name as
-- it is not specific enough. Instead we match on some bigger context which
-- is contains the located name we want to replace.
identSub :: Data a => a -> [(String, GHC.SrcSpan)] -> FunBind -> M FunBind
identSub m subs old@(GHC.FunRhs (GHC.L _ name) _ _) =
  resolveRdrName' subst (findOrError m) old subs name
  where
    subst :: FunBind -> Name -> M FunBind
    subst (GHC.FunRhs n b s) new = do
      let fakeExpr :: Pat
          fakeExpr = GHC.L (GHC.getLoc new) (GHC.VarPat noExt new)
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
               -> (AnnSpan -> M b)  -- How to find the new value, when given the location it is in
               -> a                 -- The old thing which we are going to possibly replace
               -> [(String, GHC.SrcSpan)] -- Substs
               -> GHC.RdrName       -- The name of the position in the template
                                    --we are replacing into
               -> M a
resolveRdrName' g f old subs name =
  case name of
    -- Todo: this should replace anns as well?
    GHC.Unqual (occNameString . occName -> oname)
      | Just ss <- lookup oname subs -> f (srcSpanToAnnSpan ss) >>= g old
    _ -> pure old

resolveRdrName :: (Data old, Data a)
               => a
               -> (AnnSpan -> M (GHC.Located old))
               -> GHC.Located old
               -> [(String, GHC.SrcSpan)]
               -> GHC.RdrName
               -> M (GHC.Located old)
resolveRdrName m = resolveRdrName' (modifyAnnKey m)

insertComment :: AnnKey -> String
              -> Map.Map AnnKey Annotation
              -> Map.Map AnnKey Annotation
insertComment k s as =
  let comment = Comment s badAnnSpan Nothing in
  Map.adjust (\a@Ann{..} -> a { annPriorComments = annPriorComments ++ [(comment, DP (1,0))]
                          , annEntryDelta = DP (1,0) }) k as


-- Substitute the template into the original AST.
doGenReplacement :: forall ast a. DoGenReplacement ast a
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
  | Just Refl <- eqT @(SrcSpanLess ast) @(GHC.HsDecl GHC.GhcPs)
  , GHC.L _ (GHC.ValD xvald newBind@GHC.FunBind{}) <- decomposeSrcSpan new
  , Just (oldNoLocal, oldLocal) <- stripLocalBind (decomposeSrcSpan old)
  , newLoc@(RealSrcSpan' newLocReal) <- GHC.getLoc new
  , p (composeSrcSpan oldNoLocal) = do
      (anns, keyMap) <- gets fst
      let n = decomposeSrcSpan new
          o = decomposeSrcSpan old
      (intAnns, newKeyMap) <- liftIO $ execStateT (modifyAnnKey m o n) (anns, keyMap)
      let newFile = GHC.srcSpanFile newLocReal
          newLocal = transformBi (setSrcSpanFile newFile) oldLocal
          newLocalLoc = GHC.getLoc newLocal
          ensureLoc = combineSrcSpans newLocalLoc
          newMG = GHC.fun_matches newBind
          GHC.L locMG [GHC.L locMatch newMatch] = GHC.mg_alts newMG
          newGRHSs = GHC.m_grhss newMatch
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
                    po = foldAnnKey (const False) $ \r con ->
                      srcSpanToAnnSpan (RealSrcSpan' r) == srcSpanToAnnSpan (GHC.getLoc old)
                        && con == CN "Match"
                        && GHC.srcSpanFile r /= newFile
                    pn = foldAnnKey (const False) $ \r con ->
                      srcSpanToAnnSpan (RealSrcSpan' r) == srcSpanToAnnSpan finalLoc
                        && con == CN "Match"
                        && GHC.srcSpanFile r == newFile
                in fromMaybe oldAnns $ do
                      oldAnn <- snd <$> find (po . fst) oldAnns'
                      annWhere <- find ((== G GHC.AnnWhere) . fst) (annsDP oldAnn)
                      let newSortKey = fmap (setAnnSpanFile newFile) <$> annSortKey oldAnn
                      newKey <- fst <$> find (pn . fst) oldAnns'
                      pure $ Map.adjust
                        (\ann -> ann {annsDP = annsDP ann ++ [annWhere], annSortKey = newSortKey})
                        newKey
                        oldAnns

              -- Expand the SrcSpan of the "GRHS" entry in the new file to include the local binds
              expandGRHSLoc = foldAnnKey id $ \r con ->
                if con == CN "GRHS" && GHC.srcSpanFile r == newFile
                  then AnnKey (srcSpanToAnnSpan $ ensureLoc $ RealSrcSpan' r) con
                  else AnnKey (srcSpanToAnnSpan $ RealSrcSpan' r) con

              -- If an Anns entry corresponds to the local binds, update its file to point to the new file.
              updateFile = \case
                AnnKey loc con
                  | annSpanToSrcSpan loc `GHC.isSubspanOf` GHC.getLoc oldLocal ->
                    AnnKey (setAnnSpanFile newFile loc) con
                other -> other

              -- For each SrcSpan in the new file that is the entire newLoc, set it to finalLoc
              expandTemplateLoc = \case
                AnnKey loc con
                  | loc == srcSpanToAnnSpan newLoc -> AnnKey (srcSpanToAnnSpan finalLoc) con
                other -> other

          newAnns = addLocalBindsToAnns intAnns
      put ((newAnns, newKeyMap), True)
      pure $ composeSrcSpan newWithLocalBinds
  | otherwise = pure old

-- | If the input is a FunBind with a single match, e.g., "foo a = body where x = y"
-- return "Just (foo a = body, x = y)". Otherwise return Nothing.
stripLocalBind
  :: Decl
  -> Maybe (Decl, GHC.LHsLocalBinds GHC.GhcPs)
stripLocalBind = \case
  GHC.L _ (GHC.ValD xvald origBind@GHC.FunBind{})
    | let origMG = GHC.fun_matches origBind
    , GHC.L locMG [GHC.L locMatch origMatch] <- GHC.mg_alts origMG
    , let origGRHSs = GHC.m_grhss origMatch
    , [GHC.L _ (GHC.GRHS _ _ (GHC.L loc2 _))] <- GHC.grhssGRHSs origGRHSs ->
      let loc1 = GHC.getLoc (GHC.fun_id origBind)
          newLoc = combineSrcSpans loc1 loc2
          withoutLocalBinds = setLocalBind (GHC.noLoc (GHC.EmptyLocalBinds noExt)) xvald origBind newLoc
            origMG locMG origMatch locMatch origGRHSs
       in Just (withoutLocalBinds, GHC.grhssLocalBinds origGRHSs)
  _ -> Nothing

-- | Set the local binds in a HsBind.
setLocalBind
  :: GHC.LHsLocalBinds GHC.GhcPs
  -> GHC.XValD GHC.GhcPs
  -> GHC.HsBind GHC.GhcPs
  -> GHC.SrcSpan
  -> GHC.MatchGroup GHC.GhcPs Expr
  -> GHC.SrcSpan
  -> GHC.Match GHC.GhcPs Expr
  -> GHC.SrcSpan
  -> GHC.GRHSs GHC.GhcPs Expr
  -> Decl
setLocalBind newLocalBinds xvald origBind newLoc origMG locMG origMatch locMatch origGRHSs =
    GHC.L newLoc (GHC.ValD xvald newBind)
  where
    newGRHSs = origGRHSs {GHC.grhssLocalBinds = newLocalBinds}
    newMatch = origMatch {GHC.m_grhss = newGRHSs}
    newMG = origMG {GHC.mg_alts = GHC.L locMG [GHC.L locMatch newMatch]}
    newBind = origBind {GHC.fun_matches = newMG}

replaceWorker :: forall a mod. ReplaceWorker a mod
replaceWorker as m keyMap parser seed Replace{..} = do
  let replExprLocation = srcSpanToAnnSpan pos
      uniqueName = "template" ++ show seed

  (relat, template) <- do
    flags <- maybe (withDynFlags id) pure =<< readIORef dynFlagsRef
    either (onError "replaceWorker") pure $ parser flags uniqueName orig

  (newExpr, (newAnns, newKeyMap)) <-
    runStateT
      (substTransform m subts template)
      (mergeAnns as relat, keyMap)
  let lst = listToMaybe . reverse . occNameString . rdrNameOcc
      adjacent (GHC.srcSpanEnd -> RealSrcLoc' loc1) (GHC.srcSpanStart -> RealSrcLoc' loc2) = loc1 == loc2
      adjacent _ _ = False

      -- Return @True@ if the start position of the two spans are on the same line, and differ
      -- by the given number of columns.
      diffStartCols :: Int -> GHC.SrcSpan -> GHC.SrcSpan -> Bool
      diffStartCols x (GHC.srcSpanStart -> RealSrcLoc' loc1) (GHC.srcSpanStart -> RealSrcLoc' loc2) =
        GHC.srcLocLine loc1 == GHC.srcLocLine loc2 && GHC.srcLocCol loc1 - GHC.srcLocCol loc2 == x
      diffStartCols _ _ _ = False

      -- Add a space if needed, so that we avoid refactoring `y = f(x)` into `y = fx`.
      ensureAppSpace :: Anns -> Anns
      ensureAppSpace = fromMaybe id $ do
        (GHC.L _ (GHC.HsVar _ (GHC.L _ newName))) :: Expr <- cast newExpr
        hd <- listToMaybe $ case newName of
          Unqual n -> occNameString n
          Qual moduleName _ -> GHC.moduleNameString moduleName
          Orig modu _ -> GHC.moduleNameString (GHC.moduleName modu)
          Exact name -> occNameString (nameOccName name)
        guard $ isAlphaNum hd
        let prev :: [Expr] =
              listify
                (\case
                   (GHC.L loc (GHC.HsVar _ (GHC.L _ rdr))) ->
                     maybe False isAlphaNum (lst rdr) && adjacent loc pos
                   _ -> False
                )
                m
        guard . not . null $ prev
        pure . flip Map.adjust (mkAnnKey newExpr) $ \ann ->
          if annEntryDelta ann == DP (0, 0)
            then ann { annEntryDelta = DP (0, 1) }
            else ann

      -- Add a space if needed, so that we avoid refactoring `y = do(foo bar)` into `y = dofoo bar`.
      ensureDoSpace :: Anns -> Anns
      ensureDoSpace = fromMaybe id $ do
        let doBlocks :: [Expr] =
              listify
                (\case
                  (GHC.L _ GHC.HsDo{}) -> True
                  _ -> False
                )
                m
            doBlocks' :: [(GHC.SrcSpan, Int)]
            doBlocks' =
              map
                ( \case
                    GHC.L loc (GHC.HsDo _ GHC.MDoExpr{} _) -> (loc, 3)
                    GHC.L loc _ -> (loc, 2)
                )
                doBlocks
        _ <- find (\(ss, len) -> diffStartCols len pos ss) doBlocks'
        pure . flip Map.adjust (mkAnnKey newExpr) $ \ann ->
          if annEntryDelta ann == DP (0, 0)
            then ann { annEntryDelta = DP (0, 1) }
            else ann

      replacementPred = (== replExprLocation) . getAnnSpan
      transformation = transformBiM (doGenReplacement m (replacementPred . decomposeSrcSpan) newExpr)
  runStateT (transformation m) ((newAnns, newKeyMap), False) >>= \case
    (finalM, ((ensureDoSpace . ensureAppSpace -> finalAs, finalKeyMap), True)) ->
      pure (finalAs, finalM, finalKeyMap)
    -- Failed to find a replacment so don't make any changes
    _ -> pure (as, m, keyMap)
replaceWorker as m keyMap _ _ _ = pure (as, m, keyMap)

data NotFound = NotFound
  { nfExpected :: String
  , nfActual :: Maybe String
  , nfLoc :: AnnSpan
  }

renderNotFound :: NotFound -> String
renderNotFound NotFound{..} =
  "Expected type not found at the location specified in the refact file.\n"
  ++ "  Expected type: " ++ nfExpected ++ "\n"
  ++ maybe "" (\actual -> "  Actual type: " ++ actual ++ "\n") nfActual
  ++ "  Location: " ++ showSDocUnsafe (ppr nfLoc)

-- Find a given type with a given SrcSpan
findInModule :: forall a modu. (Data a, Data modu) => modu -> AnnSpan -> Either NotFound (GHC.Located a)
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

findLargestExpression :: forall a. Data a => AnnSpan -> GHC.Located a -> Maybe (GHC.Located a)
findLargestExpression as e@(getAnnSpan -> l) = if l == as then Just e else Nothing

findOrError
  :: forall a modu m. (Data a, Data modu, MonadIO m)
  => modu -> AnnSpan -> m (GHC.Located a)
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
  :: [Extension] -> [Extension] -> FilePath -> GHC.DynFlags
  -> IO (Either String GHC.DynFlags)
addExtensionsToFlags es ds fp flags = catchErrors $ do
    (stringToStringBuffer -> buf) <- readFileUTF8' fp
    let opts = getOptions flags buf fp
        withExts = flip (foldl' xopt_unset) ds
                      . flip (foldl' xopt_set) es
                      $ flags
    (withPragmas, _, _) <- parseDynamicFilePragma withExts opts
    pure . Right $ withPragmas `gopt_set` GHC.Opt_KeepRawTokenStream
  where
    catchErrors = handleGhcException (pure . Left . show)
                . GHC.handleSourceError (pure . Left . show)

parseModuleWithArgs
  :: ([Extension], [Extension])
  -> FilePath
  -> IO (Either Errors (Anns, GHC.ParsedSource))
parseModuleWithArgs (es, ds) fp = ghcWrapper $ do
  initFlags <- initDynFlags fp
  eflags <- liftIO $ addExtensionsToFlags es ds fp initFlags
  case eflags of
    -- TODO: report error properly.
    Left err -> pure . Left $ mkErr initFlags GHC.noSrcSpan err
    Right flags -> do
      liftIO $ writeIORef' dynFlagsRef (Just flags)
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

-- TODO: This is added to avoid a breaking change. We should remove it and
-- directly pass the `DynFlags` as arguments, before the 0.10 release.
dynFlagsRef :: IORef (Maybe GHC.DynFlags)
dynFlagsRef = unsafePerformIO $ newIORef Nothing
{-# NOINLINE dynFlagsRef #-}
