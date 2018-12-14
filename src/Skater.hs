{-# LANGUAGE OverloadedStrings #-}

module Skater where

import Skater.Dev as Dev
import Skater.JsonInternal
import Skater.SkaterContext
import Skater.Log

import Data.Text.Internal as T
import Data.Set as S
import Data.Map as M
import Data.Maybe
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Types as AesonTypes

import Turtle
import Criterion.Measurement as Criterion


-- Test System
data Benchmark = UnitTest { name :: Maybe Text
                         , command :: Text
                         , outFile :: Turtle.FilePath
                         , inFile  :: Turtle.FilePath }
              | ListTest {name :: Maybe Text, testList :: [ Benchmark ] }
              | RepeatTest { nRepeat :: Int, benchmark :: Benchmark }
              deriving (Show)

-- Helpers
unnamedListTest :: [Benchmark] -> Benchmark
unnamedListTest ls = ListTest{name=Nothing  ,testList = ls}

unnamedUnitTest :: Text -> Turtle.FilePath -> Turtle.FilePath -> Benchmark 
unnamedUnitTest com outF inF = UnitTest{name=Nothing, command=com, outFile=outF, inFile=inF}

class Runner a where
  run :: SkaterContext -> a -> IO ()

exitCodeExceptionHandler :: SkaterContext -> ExitCode -> IO ()
exitCodeExceptionHandler context e = do
  logWarn context "Exit Code non zero!"
  return ()

fromShowToText :: (Show a) => a -> Text
fromShowToText = fromString . show

repeatTest :: Int -> Int -> SkaterContext -> Benchmark -> IO()
repeatTest n count context benchmark = 
  if n==count 
  then return ()
  else do
    logInfo context $ mconcat ["Repeat ", fromShowToText  count, " / ", fromShowToText n ]
    run context benchmark
    repeatTest n (count+1) context benchmark


instance Runner Benchmark where
  run context (ListTest name ls) = do
    let tName = fromMaybe "UNNAMED LISTTEST" name
    logInfo context $ mconcat [ "Launching ListTest ", tName]
    run context ls
    logInfo context $ mconcat [ "Finished ListTest ", tName]

  run context (UnitTest name command inFile oFile) = do
    let tName = fromMaybe "UNNAMED UNITTEST" name
        iStream = input inFile
    logInfo context $ mconcat [ "Launching UnitTest ", tName]
    logInfo context $ mconcat [ "Executing Command ", command]
    Criterion.initializeTime
    start <- Criterion.getTime 
    output oFile ( inshell command iStream ) `catch` exitCodeExceptionHandler context
    end <- Criterion.getTime 
    logTime context $ mconcat [tName, ";", repr ( end - start )]
    logInfo context $ mconcat [ "Finished UnitTest: ", tName]

  run context (RepeatTest nRepeat benchmark) = do
    logInfo context $ mconcat ["Launching next benchmark ", fromShowToText nRepeat, " times"]
    repeatTest nRepeat 0 context benchmark
    logInfo context $ mconcat ["Finished RepeatBenchmark ", fromShowToText nRepeat, " times"]


instance (Runner a) => Runner [a] where
  run context = Control.Monad.mapM_ (run context)


-- fetch constructor of Benchmark by type
-- Now we just must type with the proper action. 
-- The best idea would be to exploit generics and Data.data
-- for instanciating the benchmark try to map them with the type field
-- Try to work out a way that we don't have to implement any action for this 
-- operation when we add a new benchmark constructor. But for me it's too complex
-- (right now)

aesonBenchmarkMap = 
  M.fromList([
      ("UnitTest", \v -> UnitTest  <$> v .: "name" <*> v .: "command"  <*> v .: "outFile" <*> v .: "inFile")
    , ("ListTest", \v -> ListTest  <$> v .: "name" <*> v .: "testList")
    , ("RepeatTest", \v -> RepeatTest <$> v .: "nRepeat" <*> v .: "benchmark" )

  ])

instance FromJSON Benchmark where
  parseJSON (Object v) =
    let parsedName = parseMaybe (.: "type") v :: Maybe String
    in 
      if isNothing parsedName
      then error $ mconcat ["Type not valid for object", show v]
      else 
        let bType = fromJust $ parseMaybe (.: "type") v :: String
        in 
          case ( M.lookup bType aesonBenchmarkMap) of
            Just builder -> withObject bType builder (Object v)
            Nothing -> error $ mconcat ["Cannot lookup",( fromString bType) ]


-- Skip Nothing tests and run the others
instance (Runner a) => Runner (Maybe a) where
  run _ Nothing =  return ()
  run context (Just bench) = run context bench