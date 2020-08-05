{-# LANGUAGE CPP #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}

module Refact.Run where

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
import Refact.Apply
import Refact.Fixity
import Refact.Internal (apply)

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
import Data.Char
import Data.List hiding (find)
import qualified Data.List as List
import Data.Maybe
import Data.Version
import Options.Applicative
import System.IO.Extra
import System.FilePath.Find
import System.Exit
import qualified System.PosixCompat.Files as F
import Text.Read

import Paths_apply_refact

import Debug.Trace

#if __GLASGOW_HASKELL__ <= 806
type MonadFail = Monad
#endif

refactMain :: IO ()
refactMain = do
  o@Options{..} <- execParser optionsWithHelp
  when optionsVersion (putStr ("v" ++ showVersion version) >> exitSuccess)
  unless (isJust optionsTarget || isJust optionsRefactFile)
    (error "Must specify either the target file or the refact file")
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


parseVerbosity :: Monad m => String -> m Verbosity
parseVerbosity = pure . \case
  "0" -> Silent
  "1" -> Normal
  "2" -> Loud
  _   -> Normal

parsePos :: MonadFail m => String -> m (Int, Int)
parsePos s =
  case span isDigit s of
    (line, ',':col) ->
      case (,) <$> readMaybe line <*> readMaybe col of
        Just l -> pure l
        Nothing -> fail "Invalid input"
    _ -> fail "Invalid input"

data Target = StdIn | File FilePath

data Options = Options
  { optionsTarget   :: Maybe FilePath -- ^ Where to process hints
  , optionsRefactFile :: Maybe FilePath -- ^ The refactorings to process
  , optionsInplace  :: Bool
  , optionsOutput   :: Maybe FilePath -- ^ Whether to overwrite the file inplace
  , optionsVerbosity :: Verbosity
  , optionsStep :: Bool -- ^ Ask before applying each hint
  , optionsDebug :: Bool
  , optionsRoundtrip :: Bool
  , optionsVersion :: Bool
  , optionsLanguage :: [String]
  , optionsPos     :: Maybe (Int, Int)
  }

options :: Parser Options
options = do
  optionsTarget <- optional (argument str (metavar "TARGET"))
  optionsRefactFile <- option (Just <$> str) $ mconcat
    [ long "refact-file"
    , value Nothing
    , help "A file which specifies which refactorings to perform"
    ]
  optionsInplace <- switch $ mconcat
    [ long "inplace"
    , short 'i'
    , help "Whether to overwrite the target inplace"
    ]
  optionsOutput <- optional . strOption $ mconcat
    [ long "output"
    , short 'o'
    , help "Name of the file to output to"
    , metavar "FILE"
    ]
  optionsVerbosity <- option (str >>= parseVerbosity) $ mconcat
    [ long "verbosity"
    , short 'v'
    , value Normal
    , help "Specify verbosity, 0, 1 or 2. The default is 1 and 0 is silent."
    ]
  optionsStep <- switch $ mconcat
    [ short 's'
    , long "step"
    , help "Ask before applying each refactoring"
    ]
  optionsDebug <- switch $ mconcat
    [ long "debug"
    , help "Output the GHC AST for debugging"
    , internal
    ]
  optionsRoundtrip <- switch $ mconcat
    [ long "roundtrip"
    , help "Run ghc-exactprint on the file"
    , internal
    ]
  optionsVersion <- switch $ mconcat
    [ long "version"
    , help "Display version number"
    ]
  optionsLanguage <- many . strOption $ mconcat
    [ long "language"
    , short 'X'
    , help "Language extensions (e.g. LambdaCase, RankNTypes)"
    , metavar "Extensions"
    ]
  optionsPos <- option (Just <$> (str >>= parsePos)) $ mconcat
    [ long "pos"
    , value Nothing
    , metavar "<line>,<col>"
    , help "Apply hints relevant to a specific position"
    ]
  pure Options{..}

optionsWithHelp :: ParserInfo Options
optionsWithHelp = info (helper <*> options) $ mconcat
  [ fullDesc
  , progDesc "Automatically perform refactorings on haskell source files"
  , header "refactor"
  ]

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
