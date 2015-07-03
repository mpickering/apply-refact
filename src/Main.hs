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
import Language.Haskell.GHC.ExactPrint.Utils


import qualified Refact.Types as R
import Refact.Types hiding (SrcSpan)
import Refact.Perform
import Refact.Fixity
import Refact.Utils (toGhcSrcSpan, Module)
import qualified SrcLoc as GHC
import qualified DynFlags as GHC
import qualified Outputable as GHC

import Options.Applicative
import Data.Maybe
import Data.List hiding (find)
import Data.Ord

import System.Process
import System.Directory
import System.IO
import System.FilePath.Find
import System.Exit
import qualified System.PosixCompat.Files as F

import qualified Data.Map as Map

import Control.Monad
import Control.Arrow


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
  , optionsStep :: Bool -- ^ Ask before applying each hint
  , optionsDebug :: Bool
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
    <*>
    switch (short 's'
           <> help "Ask before applying each hint")
    <*>
    switch (long "debug"
           <> help "Output the GHC AST for debugging")

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
      runPipe o fp
      removeFile fp
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
      | "builds" `isPrefixOf` x = False
      | otherwise = True

filterFilename :: FindClause Bool
filterFilename = do
  ext <- extension
  fname <- fileName
  return (ext == ".hs" && p fname)
  where
    p x
      | "refactored" `isInfixOf` x = False
      | "Setup.hs" `isInfixOf` x = False
      | "HLint.hs" `isInfixOf` x = False -- HLint config files
      | "out.hs"   `isInfixOf` x = False
      | otherwise                 = True


-- Pipe

runPipe :: Options -> FilePath  -> IO ()
runPipe Options{..} file = do
  let verb = optionsVerbosity
  when (verb == Loud) (traceM $ "Parsing module")
  (anns, m) <- either (error . show) (second doFix) <$> parseModule file
  let as = relativiseApiAnns m anns
  when optionsDebug (putStrLn (showAnnData as 0 m) >> exitSuccess)
  path <- canonicalizePath file
  when (verb == Loud) (traceM $ "Getting hints from " ++ path)
  rawhints <- getHints path
  when (verb == Loud) (traceM $ "Got raw hints")
  let inp :: [(String, [Refactoring R.SrcSpan])] = read rawhints
      n = length inp
  when (verb == Loud) (traceM $ "Read " ++ show n ++ " hints")
  let refacts = (fmap . fmap . fmap) (toGhcSrcSpan file) <$> inp
      filtRefacts = removeOverlap refacts
  traceM "Applying hints"
      -- need a check here to avoid overlap
  (ares, res) <- if optionsStep
                   then foldM (uncurry refactoringLoop) (as, m) filtRefacts
                   else return $ foldl' (uncurry runRefactoring) (as, m) (concatMap snd filtRefacts)
  let output = exactPrintWithAnns res ares
  if optionsInplace && isJust optionsTarget
    then writeFile file output
    else case optionsOutput of
           Nothing -> putStrLn output
           Just f  -> do
            when (verb == Loud) (traceM $ "Writing result to " ++ f)
            writeFile f output

removeOverlap :: [(String, [Refactoring GHC.SrcSpan])] -> [(String, [Refactoring GHC.SrcSpan])]
removeOverlap ideas = map (second (filter (`notElem` bad))) ideas

  where
    bad = go rs
    rs = sortBy (comparing pos) (concatMap snd ideas)
    go [] = []
    go [x] = [x]
    go (x:y:xs) =
      if pos y `GHC.isSubspanOf` pos x
        then trace ("Ignoring " ++ showGhc (pos y) ++ " because of overlap")
             y : go (y:xs)
        else go (y:xs)

refactoringLoop :: Anns -> Module -> (String, [Refactoring GHC.SrcSpan])
                -> IO (Anns, Module)
refactoringLoop as m (desc, rs) =
  do putStrLn desc
     putStrLn "Apply hint [y, N]"
     inp <- getLine
     case inp of
          "y" -> return $ foldl' (uncurry runRefactoring) (as, m) rs
          _   -> do putStrLn "Skipping hint"
                    return (as, m)





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
  r <- hGetContents hOut
  return $! last r
  () <$ waitForProcess hProc
  return r

