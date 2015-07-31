{-# LANGUAGE RecordWildCards #-}
module Test where

import Test.Tasty
import Test.Tasty.Golden
import System.Directory
import System.FilePath

import Options.Applicative

import Main hiding (main)

import System.IO.Silently
import System.IO

import Debug.Trace
import System.Process


main =
  defaultMain =<< mkTests <$> findTests

findTests :: IO [FilePath]
findTests = do
  files <- getDirectoryContents "tests/examples"
  return $ (map ("tests/examples" </>) . filter ((== ".hs") . takeExtension )) files


mkTests :: [FilePath] -> TestTree
mkTests files = testGroup "Unit tests" (map mkTest files)
  where
    mkTest :: FilePath -> TestTree
    mkTest fp =
      let outfile = fp <.> "out"
          rfile   = fp <.> "refact"
          topts = Options
                  { optionsTarget       = Just fp
                  , optionsInplace       = False
                  , optionsOutput        = Just outfile
                  , optionsRefactFile    = Just rfile
                  , optionsSuggestions   = False
                  , optionsVerbosity     = Silent
                  , optionsStep          = False
                  , optionsRoundtrip     = False
                  , optionsDebug         = False
                  , optionsVersion       = False
                  , optionsPos           = Nothing
                  }
          action = do
            (_, str, _) <- readCreateProcessWithExitCode (shell $ "hlint --serialise " ++ fp) ""
            writeFile rfile str
            hSilence [stderr] $ runPipe topts fp
          diffCmd = \ref new -> ["diff", "-u", ref, new]
      in goldenVsFileDiff fp diffCmd (fp <.> "expected") outfile action



