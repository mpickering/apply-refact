{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Refact.Options (Options(..), optionsWithHelp) where

import Data.Char (isDigit)
import Options.Applicative
import Text.Read (readMaybe)

import Refact.Compat (MonadFail')
import Refact.Internal (Verbosity(..))

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
    , help $ "A file which specifies which refactorings to perform. "
          ++ "If not specified, it will be read from stdin, in which case TARGET must be specified."
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

parseVerbosity :: Monad m => String -> m Verbosity
parseVerbosity = pure . \case
  "0" -> Silent
  "1" -> Normal
  "2" -> Loud
  _   -> Normal

parsePos :: MonadFail' m => String -> m (Int, Int)
parsePos s =
  case span isDigit s of
    (line, ',':col) ->
      case (,) <$> readMaybe line <*> readMaybe col of
        Just l -> pure l
        Nothing -> fail "Invalid input"
    _ -> fail "Invalid input"
