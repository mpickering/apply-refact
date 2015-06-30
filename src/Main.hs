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
import Language.Haskell.GHC.ExactPrint.Types

import qualified Refact.Types as R
import Refact.Types hiding (SrcSpan)
import Refact.Perform
import Refact.Utils (toGhcSrcSpan)

import Options.Applicative

import System.Environment
import System.Process
import System.Directory
import System.IO

data Verbosity = Silent | Normal | Loud

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
  , optionsOverwrite :: Bool -- ^ Whether to overwrite the file inplace
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
      getContents >>= hPutStrLn hin
      runPipe optionsOverwrite fp
      removeFile fp
    Just target -> runPipe optionsOverwrite target




-- Pipe

runPipe :: Bool -> FilePath -> IO ()
runPipe overwrite file = do
  path <- canonicalizePath file
  rawhints <- getHints path
  let inp :: [Refactoring R.SrcSpan] = read rawhints
  print inp
  let inp' = fmap (toGhcSrcSpan file) <$> inp
  (anns, m) <- either (error . show) id <$> parseModule file
  putStrLn (showGhc anns)
  let as = relativiseApiAnns m anns
      -- need a check here to avoid overlap
  let    (ares, res) = foldl (uncurry runRefactoring) (as, m) inp'
--  putStrLn $ showGhc (Map.toAscList (fst as))
--  putStrLn $ showGhc (Map.toAscList (fst ares))
         output = exactPrintWithAnns res ares
  if overwrite
    then writeFile file output
    else putStrLn output

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

