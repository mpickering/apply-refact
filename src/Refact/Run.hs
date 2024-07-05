{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}

module Refact.Run (refactMain, runPipe) where

import Control.Monad
import Data.List hiding (find)
import Data.Maybe
import Data.Version
import Debug.Trace
import qualified GHC.Paths
import Options.Applicative
import Paths_apply_refact
import Refact.Apply (parseExtensions)
import Refact.Fixity
import Refact.Internal
  ( Verbosity (..),
    apply,
    onError,
    parseModuleWithArgs,
  )
import Refact.Options (Options (..), optionsWithHelp)
import Refact.Types hiding (SrcSpan)
import qualified Refact.Types as R
import System.Exit
import System.FilePath.Find
import System.IO.Extra
import qualified System.PosixCompat.Files as F
import Data.Functor.Identity (Identity (..))
import Language.Haskell.GHC.ExactPrint.ExactPrint (exactPrintWithOptions)
import qualified GHC
import qualified GHC.Utils.Outputable as GHC
import Refact.Compat
  ( AnnSpan,
    Errors,
    FlagSpec (..),
    FunBind,
    Module,
    -- combineSrcSpans,
    addCommentsToSrcAnn,
    combineSrcSpansA,
    composeSrcSpan,
    getOptions,
    gopt_set,
    handleGhcException,
    impliedXFlags,
    lEpaCommentRealSrcSpan,
    makeDeltaAst,
    mkAnchor,
    mkErr,
    occName,
    occNameString,
    onError,
    parseDynamicFilePragma,
    parseModuleName,
    ppr,
    refactOptions,
    setSrcSpanFile,
    showSDocUnsafe,
    srcSpanAnnDeltaPos,
    srcSpanToAnnSpan,
    stringToStringBuffer,
    transferEntryDP,
    transferEntryDP',
    xFlags,
    xopt_set,
    xopt_unset,
    pattern RealSrcSpan',
#if MIN_VERSION_ghc(9,4,0)
    mkGeneratedHsDocString,
    initParserOpts
#endif
  )
import Refact.Utils (CompleteModule (..))

-- dbg :: GHC.Outputable a => a -> String
-- dbg = GHC.showSDocUnsafe . GHC.ppr

tgt :: FilePath
tgt = "/home/zliu41/apply-refact/tests/examples/Default69.hs"

opts :: Options
opts = Options
  { -- | Where to process hints
    optionsTarget = Just tgt,
    -- | The refactorings to process
    optionsRefactFile = Just "/home/zliu41/apply-refact/tests/examples/Default69.hs.refact",
    optionsInplace = False,
    -- | Whether to overwrite the file inplace
    optionsOutput = Nothing,
    optionsVerbosity = Silent,
    -- | Ask before applying each hint
    optionsStep = False,
    optionsRoundtrip = False,
    optionsVersion = False,
    optionsLanguage  = [],
    optionsPos = Nothing
  }

refactMain :: IO ()
refactMain = do
  o@Options {..} <- execParser optionsWithHelp
  when optionsVersion (putStr ("v" ++ showVersion version) >> exitSuccess)
  unless (isJust optionsTarget || isJust optionsRefactFile) . die $
    "Must specify either the target file, or the refact file, or both.\n"
      ++ "If either the target file or the refact file is not specified, "
      ++ "it will be read from stdin.\n"
      ++ "To show usage, run 'refactor -h'."
  case optionsTarget of
    Nothing ->
      withTempFile $ \fp -> do
        getContents >>= writeFileUTF8 fp
        runPipe o fp
    Just target -> do
      targetStatus <- F.getFileStatus target
      if F.isDirectory targetStatus
        then findHsFiles target >>= mapM_ (runPipe o)
        else runPipe o target

-- Given base directory finds all haskell source files
findHsFiles :: FilePath -> IO [FilePath]
findHsFiles = find filterDirectory filterFilename

filterDirectory :: FindClause Bool
filterDirectory =
  p <$> fileName
  where
    p x
      | "." `isPrefixOf` x = False
      | otherwise = True

filterFilename :: FindClause Bool
filterFilename = do
  ext <- extension
  fname <- fileName
  pure (ext == ".hs" && p fname)
  where
    p x
      | "Setup.hs" `isInfixOf` x = False
      | otherwise = True

runPipe :: Options -> FilePath -> IO ()
runPipe Options {..} file = do
  let verb = optionsVerbosity
  rawhints <- getHints optionsRefactFile
  when (verb == Loud) (traceM "Got raw hints")
  let inp :: [(String, [Refactoring R.SrcSpan])] = read rawhints
      n = length inp
  when (verb == Loud) (traceM $ "Read " ++ show n ++ " hints")

  output <-
    if null inp
      then readFileUTF8' file
      else do
        when (verb == Loud) (traceM "Parsing module")
        let (enabledExts, disabledExts, invalidExts) = parseExtensions optionsLanguage
        unless (null invalidExts) . when (verb >= Normal) . putStrLn $
          "Invalid extensions: " ++ intercalate ", " invalidExts
        CompleteModule mSpan0 mDelta0 <-
          either (onError "runPipe") pure
            =<< parseModuleWithArgs GHC.Paths.libdir (enabledExts, disabledExts) file
        m <- CompleteModule <$> applyFixities mSpan0 <*> applyFixities mDelta0
        apply optionsPos optionsStep inp (Just file) verb m

  if optionsInplace && isJust optionsTarget
    then writeFileUTF8 file output
    else case optionsOutput of
      Nothing -> putStr output
      Just f -> do
        when (verb == Loud) (traceM $ "Writing result to " ++ f)
        writeFileUTF8 f output

getHints :: Maybe FilePath -> IO String
getHints (Just hintFile) = readFileUTF8' hintFile
getHints Nothing = getContents
