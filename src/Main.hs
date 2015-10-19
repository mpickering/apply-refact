{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}


{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Utils


import qualified Refact.Types as R
import Refact.Types hiding (SrcSpan)
import Refact.Apply
import Refact.Fixity
import Refact.Utils (toGhcSrcSpan, Module)
import qualified SrcLoc as GHC

import Options.Applicative
import Data.Maybe
import Control.Monad.Trans.Maybe
import Data.List hiding (find)
import Data.Ord

import System.Process
import System.Directory
import System.IO
import System.IO.Temp
import System.FilePath.Find
import System.Exit
import qualified System.PosixCompat.Files as F

import Control.Monad
import Control.Monad.State

import Paths_apply_refact
import Data.Version

import Debug.Trace

import SrcLoc
import Text.Read
import Data.Char

data Verbosity = Silent | Normal | Loud deriving (Eq, Show, Ord)

parseVerbosity :: Monad m => String -> m Verbosity
parseVerbosity s =
  return $ case s of
             "0" -> Silent
             "1" -> Normal
             "2" -> Loud
             _   -> Normal

parsePos :: Monad m => String -> m (Int, Int)
parsePos s =
  case span isDigit s of
    (line, ',':col) ->
      case (,) <$> readMaybe line <*> readMaybe col of
        Just l -> return l
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
  , optionsPos     :: Maybe (Int, Int)
  }

options :: Parser Options
options =
  Options <$>
    optional (argument str (metavar "TARGET"))
    <*>
    option (Just <$> str)
      (long "refact-file"
      <> value Nothing
      <> help "A file which specifies which refactorings to perform")

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
    option (str >>= parseVerbosity)
           ( long "verbosity"
           <> short 'v'
           <> value Normal
           <> help "Specify verbosity, 0, 1 or 2. The default is 1 and 0 is silent.")
    <*>
    switch (short 's'
           <> long "step"
           <> help "Ask before applying each refactoring")
    <*>
    switch (long "debug"
           <> help "Output the GHC AST for debugging"
           <> internal)
    <*>
    switch (long "roundtrip"
           <> help "Run ghc-exactprint on the file"
           <> internal)
    <*>
    switch (long "version"
           <> help "Display version number")
    <*>
    option (Just <$> (str >>= parsePos))
           (long "pos"
           <> value Nothing
           <> metavar "<line>,<col>"
           <> help "Apply hints relevant to a specific position")


optionsWithHelp :: ParserInfo Options
optionsWithHelp
  =
    info (helper <*> options)
          ( fullDesc
          <> progDesc "Automatically perform refactorings on haskell source files"
          <> header "refactor" )


main :: IO ()
main = do
  o@Options{..} <- execParser optionsWithHelp
  when optionsVersion (putStr ("v" ++ showVersion version) >> exitSuccess)
  unless (isJust optionsTarget || isJust optionsRefactFile)
    (error "Must specify either the target file or the refact file")
  case optionsTarget of
    Nothing ->
      withSystemTempFile "stdin"  (\fp hin -> do
        getContents >>= hPutStrLn hin >> hClose hin
        runPipe o fp)
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
  return (ext == ".hs" && p fname)
  where
    p x
      | "Setup.hs" `isInfixOf` x = False
      | otherwise                 = True


-- Pipe

runPipe :: Options -> FilePath  -> IO ()
runPipe Options{..} file = do
  let verb = optionsVerbosity
  when (verb == Loud) (traceM "Parsing module")
  (as, m) <- either (error . show) (uncurry applyFixities) <$> parseModule file
  when optionsDebug (putStrLn (showAnnData as 0 m))
  rawhints <- getHints optionsRefactFile
  when (verb == Loud) (traceM "Got raw hints")
  let inp :: [(String, [Refactoring R.SrcSpan])] = read rawhints
      n = length inp
  when (verb == Loud) (traceM $ "Read " ++ show n ++ " hints")
  let noOverlapInp = removeOverlap verb inp
      refacts = (fmap . fmap . fmap) (toGhcSrcSpan file) <$> noOverlapInp

      posFilter (s, rs) =
        case optionsPos of
          Nothing -> True
          Just p  -> any (flip spans p . pos) rs
      filtRefacts = filter posFilter refacts


  when (verb >= Normal) (traceM $ "Applying " ++ show (length (concatMap snd filtRefacts)) ++ " hints")
  when (verb == Loud) (traceM $ show filtRefacts)
  -- need a check here to avoid overlap
  (ares, res) <- if optionsStep
                   then fromMaybe (as, m) <$> runMaybeT (refactoringLoop as m filtRefacts)
                   else return . flip evalState 0 $
                          foldM (uncurry runRefactoring) (as, m) (concatMap snd filtRefacts)
  when (optionsDebug) (putStrLn (showAnnData ares 0 res))
  let output = exactPrint res ares
  if optionsInplace && isJust optionsTarget
    then writeFile file output
    else case optionsOutput of
           Nothing -> putStr output
           Just f  -> do
            when (verb == Loud) (traceM $ "Writing result to " ++ f)
            writeFile f output

-- Filters out overlapping ideas, picking the first idea in a set of overlapping ideas.
-- If two ideas start in the exact same place, pick the largest edit.
removeOverlap :: Verbosity -> [(String, [Refactoring R.SrcSpan])] -> [(String, [Refactoring R.SrcSpan])]
removeOverlap verb = dropOverlapping . sortBy f . summarize
  where
    -- We want to consider all Refactorings of a single idea as a unit, so compute a summary
    -- SrcSpan that encompasses all the Refactorings within each idea.
    summarize :: [(String, [Refactoring R.SrcSpan])] -> [(String, (R.SrcSpan, [Refactoring R.SrcSpan]))]
    summarize ideas = [ (s, (foldr1 summary (map pos rs), rs)) | (s, rs) <- ideas, not (null rs) ]

    summary (R.SrcSpan sl1 sc1 el1 ec1)
            (R.SrcSpan sl2 sc2 el2 ec2) =
      let (sl, sc) = case compare sl1 sl2 of
                      LT -> (sl1, sc1)
                      EQ -> (sl1, min sc1 sc2)
                      GT -> (sl2, sc2)
          (el, ec) = case compare el1 el2 of
                      LT -> (el2, ec2)
                      EQ -> (el2, max ec1 ec2)
                      GT -> (el1, ec1)
      in R.SrcSpan sl sc el ec

    -- Order by span start. If starting in same place, order by size.
    f (_,(s1,_)) (_,(s2,_)) =
      comparing startLine s1 s2 <> -- s1 first if it starts on earlier line
      comparing startCol s1 s2 <>  --             or on earlier column
      comparing endLine s2 s1 <>   -- they start in same place, s2 comes
      comparing endCol s2 s1       -- first if it ends later
      -- else, completely same span, so s1 will be first

    dropOverlapping [] = []
    dropOverlapping (p:ps) = go p ps
    go (s,(_,rs)) [] = [(s,rs)]
    go p@(s,(_,rs)) (x:xs)
      | p `overlaps` x = (if verb > Silent
                          then trace ("Ignoring " ++ show (snd (snd x)) ++ " due to overlap.")
                          else id) go p xs
      | otherwise = (s,rs) : go x xs
    -- for overlaps, we know s1 always starts <= s2, due to our sort
    overlaps (_,(s1,_)) (_,(s2,_)) =
      case compare (startLine s2) (endLine s1) of
        LT -> True
        EQ -> startCol s2 <= endCol s1
        GT -> False

data LoopOption = LoopOption
                    { desc :: String
                    , perform :: MaybeT IO (Anns, Module) }

refactoringLoop :: Anns -> Module -> [(String, [Refactoring GHC.SrcSpan])]
                -> MaybeT IO (Anns, Module)
refactoringLoop as m [] = return (as, m)
refactoringLoop as m ((_, []): rs) = refactoringLoop as m rs
refactoringLoop as m hints@((hintDesc, rs): rss) =
  do inp <- liftIO $ do
        putStrLn hintDesc
        putStrLn $ "Apply hint [" ++ intercalate ", " (map fst opts) ++ "]"
        -- In case that the input also comes from stdin
        withFile "/dev/tty" ReadMode hGetLine
     case lookup inp opts of
          Just opt   -> perform opt
          Nothing    -> loopHelp
  where
    opts =
      [ ("y", LoopOption "Apply current hint" yAction)
      , ("n", LoopOption "Don't apply the current hint" (refactoringLoop as m rss))
      , ("q", LoopOption "Apply no further hints" (return (as, m)))
      , ("d", LoopOption "Discard previous changes" mzero )
      , ("v", LoopOption "View current file" (liftIO (putStrLn (exactPrint m as))
                                              >> refactoringLoop as m hints))
      , ("?", LoopOption "Show this help menu" loopHelp)]
    loopHelp = do
            liftIO . putStrLn . unlines . map mkLine $ opts
            refactoringLoop as m hints
    mkLine (c, opt) = c ++ " - " ++ desc opt
    -- Force to force bottoms
    yAction =
      let (!r1, !r2) = flip evalState 0 $ foldM (uncurry runRefactoring) (as, m) rs
        in do
          exactPrint r2 r1 `seq` return ()
          refactoringLoop r1 r2 rss


getHints :: Maybe FilePath -> IO String
getHints (Just hintFile) = readFile hintFile
getHints Nothing = getContents

