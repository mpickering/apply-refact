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

data TestOptions = TestOptions
             { optGenerate :: Bool  -- ^ Whether to regenerate test files
             }

testOptions :: Parser TestOptions
testOptions =
  TestOptions <$>
    switch (long "generate"
           <> help "Overwrite expected test results")

testOptionsWithHelp :: ParserInfo TestOptions
testOptionsWithHelp
  = info (helper <*> testOptions)
          ( fullDesc )


main = do
  o@TestOptions{..} <- execParser testOptionsWithHelp
  setCurrentDirectory "tests/examples"
  defaultMain =<< mkTests optGenerate <$> findTests

findTests :: IO [FilePath]
findTests = do
  files <- getDirectoryContents "./"
  return $ (filter ((== ".hs") . takeExtension ) files)


mkTests :: Bool -> [FilePath] -> TestTree
mkTests gen files = testGroup "Unit tests" (map mkTest files)
  where
    mkTest :: FilePath -> TestTree
    mkTest fp =
      let outfile = fp <.> if gen then "expected" else "out"
          topts = Options
                  { optionsTarget       = Just fp
                  , optionsInplace       = False
                  , optionsOutput        = Just outfile
                  , optionsSuggestions   = False
                  , optionsHlintOptions  = ""
                  , optionsVerbosity     = Silent
                  , optionsStep          = False
                  , optionsRoundtrip     = False
                  , optionsDebug         = False
                  }
          action = hSilence [stderr] $ runPipe topts fp
          diffCmd = \ref new -> ["diff", "-u", ref, new]
      in goldenVsFileDiff fp diffCmd (fp <.> "expected") outfile action



