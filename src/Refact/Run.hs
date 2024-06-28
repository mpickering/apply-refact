{-# LANGUAGE RecordWildCards #-}

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
import Refact.Compat (showAst)
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
        m <-
          either (onError "runPipe") applyFixities
            =<< parseModuleWithArgs GHC.Paths.libdir (enabledExts, disabledExts) file
        when optionsDebug (putStrLn (showAst m))
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
