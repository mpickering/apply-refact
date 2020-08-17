{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Refact.Run (refactMain, runPipe) where

import Language.Haskell.GHC.ExactPrint
import qualified Language.Haskell.GHC.ExactPrint.Parsers as EP
  ( defaultCppOptions
  , ghcWrapper
  , initDynFlags
  , parseModuleApiAnnsWithCppInternal
  , postParseTransform
  )
import Language.Haskell.GHC.ExactPrint.Utils

import qualified Refact.Types as R
import Refact.Types hiding (SrcSpan)
import Refact.Fixity
import Refact.Internal (Errors, Verbosity(..), apply, onError, mkErr, rigidLayout)
import Refact.Options (Options(..), optionsWithHelp)

import DynFlags
import HeaderInfo (getOptions)
import HscTypes (handleSourceError)
import qualified GHC (setSessionDynFlags, ParsedSource)
import Panic (handleGhcException)
import SrcLoc
import StringBuffer (stringToStringBuffer)
import GHC.LanguageExtensions.Type (Extension(..))

import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))
import Data.List hiding (find)
import qualified Data.List as List
import Data.Maybe
import Data.Version
import Options.Applicative
import System.IO.Extra
import System.FilePath.Find
import System.Exit
import qualified System.PosixCompat.Files as F

import Paths_apply_refact

import Debug.Trace

refactMain :: IO ()
refactMain = do
  o@Options{..} <- execParser optionsWithHelp
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
      | otherwise                 = True

-- | Parse the input into a list of enabled extensions and a list of disabled extensions.
parseExtensions :: [String] -> ([Extension], [Extension])
parseExtensions = foldl' f ([], [])
  where
    f :: ([Extension], [Extension]) -> String -> ([Extension], [Extension])
    f (ys, ns) ('N' : 'o' : s) | Just ext <- readExtension s =
      (delete ext ys, ext : delete ext ns)
    f (ys, ns) s | Just ext <- readExtension s =
      (ext : delete ext ys, delete ext ns)
    -- ignore unknown extensions
    f (ys, ns) _ = (ys, ns)

    readExtension :: String -> Maybe Extension
    readExtension s = flagSpecFlag <$> List.find ((== s) . flagSpecName) xFlags

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

parseModuleWithArgs :: [String] -> FilePath -> IO (Either Errors (Anns, GHC.ParsedSource))
parseModuleWithArgs exts fp = EP.ghcWrapper $ do
  let (es, ds) = parseExtensions exts
  initFlags <- EP.initDynFlags fp
  eflags <- liftIO $ addExtensionsToFlags es ds fp initFlags
  case eflags of
    -- TODO: report error properly.
    Left err -> pure . Left $ mkErr initFlags (UnhelpfulSpan mempty) err
    Right flags -> do
      _ <- GHC.setSessionDynFlags flags
      res <- EP.parseModuleApiAnnsWithCppInternal EP.defaultCppOptions flags fp
      return $ EP.postParseTransform res rigidLayout

runPipe :: Options -> FilePath  -> IO ()
runPipe Options{..} file = do
  let verb = optionsVerbosity
  rawhints <- getHints optionsRefactFile
  when (verb == Loud) (traceM "Got raw hints")
  let inp :: [(String, [Refactoring R.SrcSpan])] = read rawhints
      n = length inp
  when (verb == Loud) (traceM $ "Read " ++ show n ++ " hints")

  output <- if null inp then readFileUTF8' file else do
    when (verb == Loud) (traceM "Parsing module")
    (as, m) <- either (onError "runPipe") (uncurry applyFixities)
                <$> parseModuleWithArgs optionsLanguage file
    when optionsDebug (putStrLn (showAnnData as 0 m))
    apply optionsPos optionsStep inp file verb as m

  if optionsInplace && isJust optionsTarget
    then writeFileUTF8 file output
    else case optionsOutput of
          Nothing -> putStr output
          Just f  -> do
            when (verb == Loud) (traceM $ "Writing result to " ++ f)
            writeFileUTF8 f output

getHints :: Maybe FilePath -> IO String
getHints (Just hintFile) = readFileUTF8' hintFile
getHints Nothing = getContents
