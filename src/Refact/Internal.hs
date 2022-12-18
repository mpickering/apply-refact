{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Refact.Internal
  ( apply,
    runRefactoring,
    addExtensionsToFlags,
    parseModuleWithArgs,
    parseExtensions,

    -- * Support for runPipe in the main process
    Verbosity (..),
    refactOptions,
    type Errors,
    onError,
    mkErr,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.State.Strict
import Data.Data
import Data.Foldable (foldlM, for_)
import Data.Functor.Identity (Identity (..))
import Data.Generics (everywhere, everywhereM, extM, listify, mkM, mkQ, mkT, something)
import Data.Generics.Uniplate.Data (transformBi, transformBiM)
import Data.IORef.Extra
import Data.List.Extra
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Tuple.Extra
import Debug.Trace
import qualified GHC
import GHC.IO.Exception (IOErrorType (..))
import GHC.LanguageExtensions.Type (Extension (..))
import qualified GHC.Paths
import Language.Haskell.GHC.ExactPrint
  ( ExactPrint,
    exactPrint,
    getEntryDP,
    makeDeltaAst,
    runTransform,
    setEntryDP,
    transferEntryDP,
    transferEntryDP',
  )
import Language.Haskell.GHC.ExactPrint.ExactPrint
  ( EPOptions,
    epRigidity,
    exactPrintWithOptions,
    stringOptions,
  )
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils (ss2pos)
import Refact.Compat
  ( AnnSpan,
    DoGenReplacement,
    Errors,
    FlagSpec (..),
    FunBind,
    Module,
    ReplaceWorker,
    -- combineSrcSpans,
    combineSrcSpansA,
    composeSrcSpan,
    getOptions,
    gopt_set,
    handleGhcException,
    impliedXFlags,
    mkErr,
    occName,
    occNameString,
    onError,
    parseDynamicFilePragma,
    parseModuleName,
    ppr,
    setSrcSpanFile,
    showSDocUnsafe,
    srcSpanToAnnSpan,
    stringToStringBuffer,
    xFlags,
    xopt_set,
    xopt_unset,
    pattern RealSrcSpan',
#if MIN_VERSION_ghc(9,4,0)
    mkGeneratedHsDocString,
    initParserOpts
#endif
  )
import Refact.Types hiding (SrcSpan)
import qualified Refact.Types as R
import Refact.Utils
  ( Decl,
    Expr,
    Import,
    M,
    Name,
    Pat,
    Stmt,
    Type,
    -- foldAnnKey,
    getAnnSpanA,
    modifyAnnKey,
    toGhcSrcSpan,
    toGhcSrcSpan',
  )
import System.IO.Error (mkIOError)
import System.IO.Extra
import System.IO.Unsafe (unsafePerformIO)

refactOptions :: EPOptions Identity String
refactOptions = stringOptions {epRigidity = RigidLayout}

-- | Apply a set of refactorings as supplied by hlint
apply ::
  Maybe (Int, Int) ->
  Bool ->
  [(String, [Refactoring R.SrcSpan])] ->
  Maybe FilePath ->
  Verbosity ->
  -- Anns ->
  Module ->
  IO String
apply mpos step inp mbfile verb m0 = do
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
        comparing startLine s1 s2
          <> comparing startCol s1 s2 -- s1 first if it starts on earlier line
          <> comparing endLine s2 s1 --             or on earlier column
          <> comparing endCol s2 s1 -- they start in same place, s2 comes
          -- first if it ends later
          -- else, completely same span, so s1 will be first
  when (verb >= Normal) . traceM $
    "Applying " ++ (show . sum . map (length . snd . fst) $ allRefacts) ++ " hints"
  when (verb == Loud) . traceM $ show (map fst allRefacts)

  m <-
    if step
      then fromMaybe m0 <$> runMaybeT (refactoringLoop m0 allRefacts)
      else evalStateT (runRefactorings verb m0 (first snd <$> allRefacts)) 0

  -- liftIO $ putStrLn $ "apply:final AST\n" ++ showAst m
  pure . snd . runIdentity $ exactPrintWithOptions refactOptions m

spans :: R.SrcSpan -> (Int, Int) -> Bool
spans R.SrcSpan {..} loc = (startLine, startCol) <= loc && loc <= (endLine, endCol)

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

runRefactorings ::
  Verbosity ->
  Module ->
  [([Refactoring GHC.SrcSpan], R.SrcSpan)] ->
  StateT Int IO Module
runRefactorings verb m0 ((rs, ss) : rest) = do
  runRefactorings' verb m0 rs >>= \case
    Nothing -> runRefactorings verb m0 rest
    Just m -> do
      let (overlaps, rest') = span (overlap ss . snd) rest
      when (verb >= Normal) . for_ overlaps $ \(rs', _) ->
        traceM $ "Ignoring " ++ show rs' ++ " due to overlap."
      runRefactorings verb m rest'
runRefactorings _ m [] = pure m

runRefactorings' ::
  Verbosity ->
  Module ->
  [Refactoring GHC.SrcSpan] ->
  StateT Int IO (Maybe Module)
runRefactorings' verb m0 rs = do
  seed <- get
  m <- foldlM runRefactoring m0 rs
  if droppedComments rs m0 m
    then do
      put seed
      when (verb >= Normal) . traceM $
        "Ignoring " ++ show rs ++ " since applying them would cause comments to be dropped."
      pure Nothing
    else pure $ Just m

overlap :: R.SrcSpan -> R.SrcSpan -> Bool
overlap s1 s2 =
  -- We know s1 always starts <= s2, due to our sort
  case compare (startLine s2) (endLine s1) of
    LT -> True
    EQ -> startCol s2 <= endCol s1
    GT -> False

data LoopOption = LoopOption
  { desc :: String,
    -- perform :: MaybeT IO (Anns, Module)
    perform :: MaybeT IO Module
  }

refactoringLoop ::
  Module ->
  [((String, [Refactoring GHC.SrcSpan]), R.SrcSpan)] ->
  MaybeT IO Module
refactoringLoop m [] = pure m
refactoringLoop m (((_, []), _) : rs) = refactoringLoop m rs
refactoringLoop m0 hints@(((hintDesc, rs), ss) : rss) = do
  res <- liftIO . flip evalStateT 0 $ runRefactorings' Silent m0 rs
  let yAction :: MaybeT IO Module
      yAction = case res of
        Just m -> do
          exactPrint m `seq` pure ()
          refactoringLoop m $ dropWhile (overlap ss . snd) rss
        Nothing -> do
          liftIO $ putStrLn "Hint skipped since applying it would cause comments to be dropped"
          refactoringLoop m0 rss
      opts :: [(String, LoopOption)]
      opts =
        [ ("y", LoopOption "Apply current hint" yAction),
          ("n", LoopOption "Don't apply the current hint" (refactoringLoop m0 rss)),
          ("q", LoopOption "Apply no further hints" (pure m0)),
          ("d", LoopOption "Discard previous changes" mzero),
          ( "v",
            LoopOption
              "View current file"
              ( liftIO (putStrLn (exactPrint m0))
                  >> refactoringLoop m0 hints
              )
          ),
          ("?", LoopOption "Show this help menu" loopHelp)
        ]
      loopHelp = do
        liftIO . putStrLn . unlines . map mkLine $ opts
        refactoringLoop m0 hints
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
runRefactoring ::
  Data a =>
  a ->
  Refactoring GHC.SrcSpan ->
  StateT Int IO a
runRefactoring m = \case
  r@Replace {} -> do
    seed <- get <* modify (+ 1)
    liftIO $ case rtype r of
      Expr -> replaceWorker m parseExpr seed r
      Decl -> replaceWorker m parseDecl seed r
      Type -> replaceWorker m parseType seed r
      Pattern -> replaceWorker m parsePattern seed r
      Stmt -> replaceWorker m parseStmt seed r
      Bind -> replaceWorker m parseBind seed r
      R.Match -> replaceWorker m parseMatch seed r
      ModuleName -> replaceWorker m (parseModuleName (pos r)) seed r
      Import -> replaceWorker m parseImport seed r
  ModifyComment {..} -> pure (modifyComment pos newComment m)
  Delete {rtype, pos} -> pure (f m)
    where
      annSpan = srcSpanToAnnSpan pos
      f = case rtype of
        Stmt -> doDeleteStmt ((/= annSpan) . getAnnSpanA)
        Import -> doDeleteImport ((/= annSpan) . getAnnSpanA)
        _ -> id
  InsertComment {..} -> pure (addComment m)
    where
      addComment = transformBi go
      r = srcSpanToAnnSpan pos
      go :: GHC.LHsDecl GHC.GhcPs -> GHC.LHsDecl GHC.GhcPs
      go old@(GHC.L l d) =
        if ss2pos (srcSpanToAnnSpan $ GHC.locA l) == ss2pos r
          then
            let dp = case getEntryDP old of
                  GHC.SameLine 0 -> GHC.DifferentLine 1 0
                  dp' -> dp'
                (GHC.L l' d') = setEntryDP (GHC.L l d) (GHC.DifferentLine 1 0)
                comment =
                  GHC.L
                    (GHC.Anchor r (GHC.MovedAnchor dp))
                    (GHC.EpaComment (GHC.EpaLineComment newComment) r)
                l'' = GHC.addCommentsToSrcAnn l' (GHC.EpaComments [comment])
             in GHC.L l'' d'
          else old
  RemoveAsKeyword {..} -> pure (removeAsKeyword m)
    where
      removeAsKeyword = transformBi go
      go :: GHC.LImportDecl GHC.GhcPs -> GHC.LImportDecl GHC.GhcPs
      go imp@(GHC.L l i)
        | srcSpanToAnnSpan (GHC.locA l) == srcSpanToAnnSpan pos = GHC.L l (i {GHC.ideclAs = Nothing})
        | otherwise = imp

modifyComment :: (Data a) => GHC.SrcSpan -> String -> a -> a
modifyComment pos newComment = transformBi go
  where
#if MIN_VERSION_ghc(9,4,0)
    newTok :: GHC.EpaCommentTok -> GHC.EpaCommentTok
    newTok (GHC.EpaDocComment _) = GHC.EpaDocComment $ mkGeneratedHsDocString newComment
    newTok (GHC.EpaDocOptions _) = GHC.EpaDocOptions newComment
    newTok (GHC.EpaLineComment _) = GHC.EpaLineComment newComment
    newTok (GHC.EpaBlockComment _) = GHC.EpaBlockComment newComment
    newTok GHC.EpaEofComment = GHC.EpaEofComment
#else
    newTok (GHC.EpaDocCommentNext _) = GHC.EpaDocCommentNext newComment
    newTok (GHC.EpaDocCommentPrev _) = GHC.EpaDocCommentPrev newComment
    newTok (GHC.EpaDocCommentNamed _) = GHC.EpaDocCommentNamed newComment
    newTok (GHC.EpaDocSection i _) = GHC.EpaDocSection i newComment
    newTok (GHC.EpaDocOptions _) = GHC.EpaDocOptions newComment
    newTok (GHC.EpaLineComment _) = GHC.EpaLineComment newComment
    newTok (GHC.EpaBlockComment _) = GHC.EpaBlockComment newComment
    newTok GHC.EpaEofComment = GHC.EpaEofComment
#endif

    go :: GHC.LEpaComment -> GHC.LEpaComment
    go old@(GHC.L (GHC.Anchor l o) (GHC.EpaComment t r)) =
      if ss2pos l == ss2pos (GHC.realSrcSpan pos)
        then GHC.L (GHC.Anchor l o) (GHC.EpaComment (newTok t) r)
        else old

droppedComments :: [Refactoring GHC.SrcSpan] -> Module -> Module -> Bool
droppedComments rs orig_m m = not (all (`elem` current_comments) orig_comments)
  where
    mcs = foldl' runModifyComment orig_m rs
    runModifyComment m' (ModifyComment pos newComment) = modifyComment pos newComment m'
    runModifyComment m' _ = m'

    all_comments :: forall r. (Data r, Typeable r) => r -> [GHC.EpaComment]
    all_comments = listify (False `mkQ` isComment)
    isComment :: GHC.EpaComment -> Bool
    isComment _ = True
    orig_comments = all_comments mcs
    current_comments = all_comments m

parseBind :: Parser (GHC.LHsBind GHC.GhcPs)
parseBind dyn fname s =
  case parseDecl dyn fname s of
    -- Safe as we add no annotations to the ValD
    Right (GHC.L l (GHC.ValD _ b)) -> Right (GHC.L l b)
    Right (GHC.L l _) -> Left (mkErr dyn (GHC.locA l) "Not a HsBind")
    Left e -> Left e

parseMatch :: Parser (GHC.LMatch GHC.GhcPs (GHC.LHsExpr GHC.GhcPs))
parseMatch dyn fname s =
  case parseBind dyn fname s of
    Right (GHC.L l GHC.FunBind {fun_matches}) ->
      case GHC.unLoc (GHC.mg_alts fun_matches) of
        [x] -> Right x
        _ -> Left (mkErr dyn (GHC.locA l) "Not a single match")
    Right (GHC.L l _) -> Left (mkErr dyn (GHC.locA l) "Not a funbind")
    Left e -> Left e

-- Substitute variables into templates
-- Finds places in the templates where we need to insert variables.

substTransform :: (Data a, Data b) => b -> [(String, GHC.SrcSpan)] -> a -> M a
substTransform m ss =
  everywhereM
    ( mkM (typeSub m ss)
        `extM` identSub m ss
        `extM` patSub m ss
        `extM` stmtSub m ss
        `extM` exprSub m ss
    )

stmtSub :: Data a => a -> [(String, GHC.SrcSpan)] -> Stmt -> M Stmt
stmtSub m subs old@(GHC.L _ (GHC.BodyStmt _ (GHC.L _ (GHC.HsVar _ (GHC.L _ name))) _ _)) =
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
    subst (GHC.FunRhs _ b s) new = do
      -- Low level version as we need to combine the annotation information
      -- from the template RdrName and the original VarPat.
      -- modify . first $
      --   replaceAnnKey (mkAnnKey n) (mkAnnKey fakeExpr) (mkAnnKey new) (mkAnnKey fakeExpr)
      pure $ GHC.FunRhs new b s
    subst o _ = pure o
identSub _ _ e = pure e

-- g is usually modifyAnnKey
-- f is usually a function which checks the locations are equal
resolveRdrName' ::
  (a -> GHC.LocatedAn an b -> M a) -> -- How to combine the value to insert and the replaced value
  (AnnSpan -> M (GHC.LocatedAn an b)) -> -- How to find the new value, when given the location it is in
  a -> -- The old thing which we are going to possibly replace
  [(String, GHC.SrcSpan)] -> -- Substs
  GHC.RdrName -> -- The name of the position in the template
  --we are replacing into
  M a
resolveRdrName' g f old subs name =
  case name of
    -- Todo: this should replace anns as well?
    GHC.Unqual (occNameString . occName -> oname)
      | Just ss <- lookup oname subs -> f (srcSpanToAnnSpan ss) >>= g old
    _ -> pure old

resolveRdrName ::
  (Data old, Data a, Data an, Typeable an, Monoid an) =>
  a ->
  (AnnSpan -> M (GHC.LocatedAn an old)) ->
  GHC.LocatedAn an old ->
  [(String, GHC.SrcSpan)] ->
  GHC.RdrName ->
  M (GHC.LocatedAn an old)
resolveRdrName m = resolveRdrName' (modifyAnnKey m)

-- Substitute the template into the original AST.
doGenReplacement :: forall ast a. DoGenReplacement GHC.AnnListItem ast a
doGenReplacement _ p new old
  | p old = do
    let (new', _, _) = runTransform $ transferEntryDP old new
    put True
    pure new'
  -- If "f a = body where local" doesn't satisfy the predicate, but "f a = body" does,
  -- run the replacement on "f a = body", and add "local" back afterwards.
  -- This is useful for hints like "Eta reduce" and "Redundant where".
  | Just Refl <- eqT @(GHC.LocatedA ast) @(GHC.LHsDecl GHC.GhcPs),
    GHC.L _ (GHC.ValD xvald newBind@GHC.FunBind {}) <- new,
    Just (oldNoLocal, oldLocal) <- stripLocalBind old,
    (RealSrcSpan' newLocReal) <- GHC.getLocA new,
    p (composeSrcSpan oldNoLocal) = do
    let newFile = GHC.srcSpanFile newLocReal
        newLocal :: GHC.HsLocalBinds GHC.GhcPs
        newLocal = transformBi (setSrcSpanFile newFile) oldLocal
        -- newLocalLoc = GHC.getLocA newLocal
        newLocalLoc = GHC.spanHsLocaLBinds newLocal
        newMG = GHC.fun_matches newBind
        GHC.L locMG [GHC.L locMatch newMatch] = GHC.mg_alts newMG
        newGRHSs = GHC.m_grhss newMatch
        finalLoc = combineSrcSpansA (GHC.noAnnSrcSpan newLocalLoc) (GHC.getLoc new)
        newWithLocalBinds0 =
          setLocalBind
            newLocal
            xvald
            newBind
            finalLoc
            newMG
            (combineSrcSpansA (GHC.noAnnSrcSpan newLocalLoc) locMG)
            newMatch
            (combineSrcSpansA (GHC.noAnnSrcSpan newLocalLoc) locMatch)
            newGRHSs
        (newWithLocalBinds, _, _) = runTransform $ transferEntryDP' old newWithLocalBinds0

    put True
    pure $ composeSrcSpan newWithLocalBinds
  | otherwise = pure old

-- | If the input is a FunBind with a single match, e.g., "foo a = body where x = y"
-- return "Just (foo a = body, x = y)". Otherwise return Nothing.
stripLocalBind ::
  Decl ->
  Maybe (Decl, GHC.HsLocalBinds GHC.GhcPs)
stripLocalBind = \case
  GHC.L _ (GHC.ValD xvald origBind@GHC.FunBind {})
    | let origMG = GHC.fun_matches origBind,
      GHC.L locMG [GHC.L locMatch origMatch] <- GHC.mg_alts origMG,
      let origGRHSs = GHC.m_grhss origMatch,
      [GHC.L _ (GHC.GRHS _ _ (GHC.L loc2 _))] <- GHC.grhssGRHSs origGRHSs ->
      let loc1 = GHC.getLoc (GHC.fun_id origBind)
          newLoc = combineSrcSpansA (GHC.l2l loc1) loc2
          withoutLocalBinds =
            setLocalBind
              (GHC.EmptyLocalBinds GHC.noExtField)
              xvald
              origBind
              newLoc
              origMG
              locMG
              origMatch
              locMatch
              origGRHSs
       in Just (withoutLocalBinds, GHC.grhssLocalBinds origGRHSs)
  _ -> Nothing

-- | Set the local binds in a HsBind.
setLocalBind ::
  GHC.HsLocalBinds GHC.GhcPs ->
  GHC.XValD GHC.GhcPs ->
  GHC.HsBind GHC.GhcPs ->
  GHC.SrcSpanAnnA ->
  GHC.MatchGroup GHC.GhcPs Expr ->
  GHC.SrcSpanAnnL ->
  GHC.Match GHC.GhcPs Expr ->
  GHC.SrcSpanAnnA ->
  GHC.GRHSs GHC.GhcPs Expr ->
  Decl
setLocalBind newLocalBinds xvald origBind newLoc origMG locMG origMatch locMatch origGRHSs =
  GHC.L newLoc (GHC.ValD xvald newBind)
  where
    newGRHSs = origGRHSs {GHC.grhssLocalBinds = newLocalBinds}
    newMatch = origMatch {GHC.m_grhss = newGRHSs}
    newMG = origMG {GHC.mg_alts = GHC.L locMG [GHC.L locMatch newMatch]}
    newBind = origBind {GHC.fun_matches = newMG}

replaceWorker :: forall a mod. (ExactPrint a) => ReplaceWorker a mod
replaceWorker m parser seed Replace {..} = do
  let replExprLocation = srcSpanToAnnSpan pos
      uniqueName = "template" ++ show seed
  let libdir = undefined

  template <- do
    flags <- maybe (withDynFlags libdir id) pure =<< readIORef dynFlagsRef
    either (onError "replaceWorker") pure $ parser flags uniqueName orig

  (newExpr, ()) <-
    runStateT
      -- (substTransform m subts template)
      (substTransform m subts (makeDeltaAst template))
      -- (mergeAnns as relat, keyMap)
      ()
  -- Add a space if needed, so that we avoid refactoring `y = do(foo bar)` into `y = dofoo bar`.
  -- ensureDoSpace :: Anns -> Anns
  let ensureSpace :: forall t. (Data t) => t -> t
      ensureSpace = everywhere (mkT ensureExprSpace)

      ensureExprSpace :: Expr -> Expr
      ensureExprSpace e@(GHC.L l (GHC.HsDo an v (GHC.L ls stmts))) = e' -- ensureDoSpace
        where
          isDo = case v of
            GHC.ListComp -> False
            _ -> True
          e' =
            if isDo
              && manchorOp an == Just (GHC.MovedAnchor (GHC.SameLine 0))
              && manchorOp (GHC.ann ls) == Just (GHC.MovedAnchor (GHC.SameLine 0))
              then GHC.L l (GHC.HsDo an v (setEntryDP (GHC.L ls stmts) (GHC.SameLine 1)))
              else e
      ensureExprSpace e@(GHC.L l (GHC.HsApp x (GHC.L la a) (GHC.L lb b))) = e' -- ensureAppSpace
        where
          e' =
            if manchorOp (GHC.ann lb) == Just (GHC.MovedAnchor (GHC.SameLine 0))
              then GHC.L l (GHC.HsApp x (GHC.L la a) (setEntryDP (GHC.L lb b) (GHC.SameLine 1)))
              else e
      ensureExprSpace e = e

      replacementPred = (== replExprLocation) . getAnnSpanA

      tt :: GHC.LocatedA a -> StateT Bool IO (GHC.LocatedA a)
      tt = doGenReplacement m replacementPred newExpr
      transformation :: mod -> StateT Bool IO mod
      transformation = transformBiM tt
  runStateT (transformation m) False >>= \case
    (finalM, True) ->
      pure (ensureSpace finalM)
    -- Failed to find a replacment so don't make any changes
    _ -> pure m
replaceWorker m _ _ _ = pure m

manchorOp :: GHC.EpAnn ann -> Maybe GHC.AnchorOperation
manchorOp GHC.EpAnnNotUsed = Nothing
manchorOp (GHC.EpAnn a _ _) = Just (GHC.anchor_op a)

data NotFound = NotFound
  { nfExpected :: String,
    nfActual :: Maybe String,
    nfLoc :: AnnSpan
  }

renderNotFound :: NotFound -> String
renderNotFound NotFound {..} =
  "Expected type not found at the location specified in the refact file.\n"
    ++ "  Expected type: "
    ++ nfExpected
    ++ "\n"
    ++ maybe "" (\actual -> "  Actual type: " ++ actual ++ "\n") nfActual
    ++ "  Location: "
    ++ showSDocUnsafe (ppr nfLoc)

-- Find a given type with a given SrcSpan
findInModule ::
  forall an a modu.
  (Typeable an, Data a, Data modu) =>
  modu ->
  AnnSpan ->
  Either NotFound (GHC.LocatedAn an a)
findInModule m ss = case doTrans m of
  Just a -> Right a
  Nothing ->
    let expected = show (typeRep (Proxy @a))
        actual =
          listToMaybe $
            catMaybes
              [ showType (doTrans m :: Maybe Expr),
                showType (doTrans m :: Maybe Type),
                showType (doTrans m :: Maybe Decl),
                showType (doTrans m :: Maybe Pat),
                showType (doTrans m :: Maybe Name)
              ]
     in Left $ NotFound expected actual ss
  where
    doTrans :: forall an' b. (Typeable an', Data b) => modu -> Maybe (GHC.LocatedAn an' b)
    doTrans = something (mkQ Nothing (findLargestExpression ss))

    showType :: forall an' b. Typeable b => Maybe (GHC.LocatedAn an' b) -> Maybe String
    showType = fmap $ \_ -> show (typeRep (Proxy @b))

findLargestExpression ::
  forall an a.
  Data a =>
  AnnSpan ->
  GHC.LocatedAn an a ->
  Maybe (GHC.LocatedAn an a)
findLargestExpression as e@(getAnnSpanA -> l) = if l == as then Just e else Nothing

findOrError ::
  forall a an modu m.
  (Typeable an, Data a, Data modu, MonadIO m) =>
  modu ->
  AnnSpan ->
  m (GHC.LocatedAn an a)
findOrError m = either f pure . findInModule m
  where
    f nf = liftIO . throwIO $ mkIOError InappropriateType (renderNotFound nf) Nothing Nothing

-- Deletion from a list

doDeleteStmt :: Data a => (Stmt -> Bool) -> a -> a
doDeleteStmt = transformBi . filter

doDeleteImport :: Data a => (Import -> Bool) -> a -> a
doDeleteImport = transformBi . filter

addExtensionsToFlags ::
  [Extension] ->
  [Extension] ->
  FilePath ->
  GHC.DynFlags ->
  IO (Either String GHC.DynFlags)
addExtensionsToFlags es ds fp flags = catchErrors $ do
  (stringToStringBuffer -> buf) <- readFileUTF8' fp
#if MIN_VERSION_ghc(9,4,0)
  let (_, opts) = getOptions (initParserOpts flags) buf fp
#else
  let opts = getOptions flags buf fp
#endif
      withExts =
        flip (foldl' xopt_unset) ds
          . flip (foldl' xopt_set) es
          $ flags
  (withPragmas, _, _) <- parseDynamicFilePragma withExts opts
  pure . Right $ withPragmas `gopt_set` GHC.Opt_KeepRawTokenStream
  where
    catchErrors =
      handleGhcException (pure . Left . show)
        . GHC.handleSourceError (pure . Left . show)

parseModuleWithArgs ::
  ([Extension], [Extension]) ->
  FilePath ->
  IO (Either Errors GHC.ParsedSource)
parseModuleWithArgs (es, ds) fp = ghcWrapper GHC.Paths.libdir $ do
  initFlags <- initDynFlags fp
  eflags <- liftIO $ addExtensionsToFlags es ds fp initFlags
  case eflags of
    -- TODO: report error properly.
    Left err -> pure . Left $ mkErr initFlags GHC.noSrcSpan err
    Right flags -> do
      liftIO $ writeIORef' dynFlagsRef (Just flags)
      res <- parseModuleEpAnnsWithCppInternal defaultCppOptions flags fp

      -- pure $ postParseTransform res rigidLayout
      case postParseTransform res of
        Left e -> pure (Left e)
        Right ast -> pure $ Right (makeDeltaAst ast)

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
    f (ys, ns, is) ('N' : 'o' : s)
      | Just ext <- readExtension s =
        (delete ext ys, ext : delete ext ns, is)
    f (ys, ns, is) s
      | Just ext <- readExtension s =
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
