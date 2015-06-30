{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}


{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Language.Haskell.GHC.ExactPrint hiding (Parser)

import qualified Refact.Types as R
import Refact.Types hiding (SrcSpan)
import Refact.Perform
import Refact.Utils (toGhcSrcSpan)

import Options.Applicative
import Data.Maybe
import Data.List

import System.Process
import System.Directory
import System.IO

import Control.Monad

import Debug.Trace

data Verbosity = Silent | Normal | Loud deriving (Eq, Show, Ord)

parseVerbosity :: Monad m => String -> m Verbosity
parseVerbosity s =
  return $ case s of
             "0" -> Silent
             "1" -> Normal
             "2" -> Loud
             _   -> Normal

data Target = StdIn | File FilePath

data Options = Options
  { optionsTarget :: Maybe FilePath -- ^ Where to process hints
  , optionsInplace :: Bool
  , optionsOutput :: Maybe FilePath -- ^ Whether to overwrite the file inplace
  , optionsSuggestions :: Bool -- ^ Whether to perform suggestions
  , optionsHlintOptions :: String -- ^ Commands to pass to hlint
  , optionsVerbosity :: Verbosity
  }

options :: Parser Options
options =
  Options <$>
    optional (argument str (metavar "TARGET"))
    <*>
    switch (long "inplace"
           <> short 'i'
           <> help "Whether to overwrite the target inplace")
    <*>
    optional (strOption (long "output"
                        <> short 'o'
                        <> help "Name of the file to output to"
                        <> metavar "FILE"))
    <*>
    switch (long "replace-suggestions"
           <> help "Whether to process suggestions as well as errors")
    <*>
    strOption (long "hlint-options"
              <> help "Options to pass to hlint"
              <> metavar "OPTS"
              <> value "" )
    <*>
    option (str >>= parseVerbosity)
           ( long "verbosity"
           <> short 'v'
           <> value Normal
           <> help "Enable verbose mode")

optionsWithHelp :: ParserInfo Options
optionsWithHelp
  = info (helper <*> options)
          ( fullDesc
          <> progDesc "Automatically perform hlint suggestions"
          <> header "hlint-refactor" )




main :: IO ()
main = do
  o@Options{..} <- execParser optionsWithHelp
  case optionsTarget of
    Nothing -> do
      (fp, hin) <- openTempFile "./" "stdin"
      getContents >>= hPutStrLn hin >> hClose hin
      runPipe fp o
      removeFile fp
    Just target -> runPipe target o


-- Pipe

runPipe :: FilePath -> Options -> IO ()
runPipe file Options{..} = do
  let verb = optionsVerbosity
  path <- canonicalizePath file
  when (verb == Loud) (traceM $ "Getting hints from " ++ path)
  rawhints <- getHints path
  when (verb == Loud) (traceM $ "Got raw hints")
  let inp :: [Refactoring R.SrcSpan] = read rawhints
      n = length inp
  when (verb == Loud) (traceM $ "Read " ++ show n ++ " hints")
  let inp' = fmap (toGhcSrcSpan file) <$> inp
  when (verb == Loud) (traceM $ "Parsing module")
  (anns, m) <- either (error . show) id <$> parseModule file
  traceM "Applying hints"
  let as = relativiseApiAnns m anns
      -- need a check here to avoid overlap
      (ares, res) = foldl' (uncurry runRefactoring) (as, m) inp'
      output = exactPrintWithAnns res ares
  if optionsInplace && isJust optionsTarget
    then writeFile file output
    else case optionsOutput of
           Nothing -> putStrLn output
           Just f  -> do
            when (verb == Loud) (traceM $ "Writing result to " ++ f)
            writeFile f output

-- Run HLint to get the commands

makeCmd :: String -> String
makeCmd file = "hlint " ++ file ++ " --serialise"
--makeCmd _ = "runghc-7.10.1.20150609 -package-db=.cabal-sandbox/x86_64-osx-ghc-7.10.1.20150609-packages.conf.d/ feeder.hs"

getHints :: FilePath -> IO String
getHints file = do
  (_, Just hOut, _, hProc) <- createProcess (
                                (shell (makeCmd file))
                                { std_out = CreatePipe }
                              )
  () <$ waitForProcess hProc
  hGetContents hOut

