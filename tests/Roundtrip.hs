{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.FilePath

import Data.List hiding (find)

import System.Exit

import System.Directory

import Test.HUnit hiding (failures)

import System.FilePath.Find

import Debug.Trace
import Control.Monad
import System.Environment

import qualified Data.Set as S

import System.IO (hClose, hPutStr, openTempFile)
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Parsers

import System.Process

import Data.Algorithm.Diff (getDiff)
import Data.Algorithm.DiffOutput (ppDiff)

import Control.Exception

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

processed = "processed.txt"

failures = "failures.txt"

reparse = "reparsed.txt"


writeProcessed :: FilePath -> IO ()
writeProcessed fp = appendFile processed (('\n' : fp))

writeAppFailure :: String -> IO ()
writeAppFailure s = appendFile failures (('\n': s))


writeRepFailure :: String -> IO ()
writeRepFailure s = appendFile reparse (('\n': s))



main :: IO ()
main = do
  as <- getArgs
  case as of
    [] -> putStrLn "Must enter directory to process"
    ["failures"] -> do
      fs <- lines <$> readFile "origfailures.txt"
      () <$ runTests (TestList (map mkParserTest fs))
    ["clean"] -> do
      putStrLn "Cleaning..."
      writeFile processed ""
      writeFile failures ""
      writeFile reparse ""
      removeDirectoryRecursive "tests/roundtrip"
      createDirectory "tests/roundtrip"
      putStrLn "Done."
    ds -> () <$ (runTests =<< (TestList <$> mapM tests ds))

runTests :: Test -> IO Counts
runTests t = do
  let n = testCaseCount t
  putStrLn $ "Running " ++ show n ++ " tests."
  runTestTT t

tests :: FilePath -> IO Test
tests dir = do
  done <- S.fromList . lines <$> readFile processed
  roundTripHackage done dir

-- Selection:

-- Given base directory finds all haskell source files
findSrcFiles :: FilePath -> IO [FilePath]
findSrcFiles = find filterDirectory filterFilename

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
  return (ext == ".hs" && p fname)
  where
    p x
      | "refactored" `isInfixOf` x = False
      | "Setup.hs" `isInfixOf` x = False
      | "HLint.hs" `isInfixOf` x = False -- HLint config files
      | "out.hs"   `isInfixOf` x = False
      | "/builds/" `isInfixOf` x = False
      | otherwise                 = True

-- Hackage dir
roundTripHackage :: S.Set String -> FilePath -> IO Test
roundTripHackage done hackageDir = do
  packageDirs <- drop 2 <$> getDirectoryContents hackageDir
  TestList <$> mapM (roundTripPackage done) (zip [0..] (map (hackageDir </>) packageDirs))


roundTripPackage :: S.Set String -> (Int, FilePath) -> IO Test
roundTripPackage done (n, dir) = do
  putStrLn (show n)
  hsFiles <- filter (flip S.notMember done)  <$> findSrcFiles dir

  return (TestLabel (dropFileName dir) (TestList $ map mkParserTest hsFiles))

mkParserTest :: FilePath -> Test
mkParserTest fp =
    TestCase (do writeProcessed fp
                 r <- robustTest fp
                 case rep r of
                  Failure s -> writeAppFailure (file r ++ "\n" ++ s) >> exitFailure
                  Success v -> do
                    case v of
                      Just e -> writeRepFailure e
                      Nothing -> do
                        old <- readFile fp
                        new <- readFile (fp <.> "out" <.> "hs")
                        let tokenize = map (:[]) . lines
                            diff = getDiff (tokenize old) (tokenize new)
                            res  = ppDiff diff
                        unless (res == "\n") (writeFailure fp res)
                    assertBool fp (Nothing == v))

call :: FilePath -> IO ()
call fp = callCommand ("refactor -o " ++ out ++ " " ++ fp)
  where
    out = fp <.> "out" <.> "hs"

data Rep = Failure String | Success (Maybe String)


data Report = Report
     { rep :: Rep
     , file   :: FilePath
     }

robustTest :: FilePath -> IO Report
robustTest fp = do
  res <- catchAny (Nothing <$ call fp) (\e -> return (Just $ show e))
  case res of
    Just e -> return $ Report (Failure e) fp
    Nothing -> do
      catchAny (parseModule (fp <.> "out" <.> "hs") >>=
                \case
                  Left e -> return $ Report (Success (Just $ show e)) fp
                  Right _ -> return $ Report (Success Nothing) fp)
               (\_ -> return $ Report (Success (Just $ fp))  fp)


writeFailure :: FilePath -> String -> IO ()
writeFailure fp db = do
  let outdir      = "tests" </> "roundtrip"
      outname     = takeFileName fp <.> "out"
  (fname, handle) <- openTempFile outdir outname
  (hPutStr handle db >> hClose handle)
