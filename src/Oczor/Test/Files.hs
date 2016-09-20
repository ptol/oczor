module Oczor.Test.Files where

import System.Directory
import Data.List.Split
import ClassyPrelude hiding (try)
import Oczor.Utl

import qualified System.FilePath as Fp
import qualified System.IO.Strict as S

type Test = (Int,(String, String))
type TestSet = (String, [Test])
type CheckFunc = String -> Either String String

root = "tests/"
testsFileExt = ".txt"

strip :: Char -> String -> String
strip x = stripL x . stripR x where
  stripL :: Char -> String -> String
  stripL x = dropWhile (==x)

  stripR :: Char -> String -> String
  stripR x = reverse . stripL x . reverse

parseTestTexts txt = (strip '\n' $ unsafeIndex l2 0, strip '\n' $ unsafeIndex l2 1)
  where
    l = splitOn "\n-" txt 
    len = length l
    l2 = case len of
      1 -> l ++ ["???"]
      2 -> l
      _ -> error $ unlines ["wrong format", txt]

parseTests txt = splitOn "\n=" txt <&> (parseTestTexts . strip '\n') & zip [1..]

readTestSet :: FilePath -> IO TestSet
readTestSet file = do
  txt <- S.readFile file
  return (file, parseTests txt)

newTestSetText f (input, output) = (input, f input & (\(Right x) -> x))

dirTestSets d = do
  let dir = root ++ d
  files <- getTestFiles dir
  traverse (\x -> readTestSet (dir ++ "/" ++ x)) files

findTestSetFilePath dir testsName = do
  let dirPath = root ++ dir ++ "/"
  let file = dirPath ++ testsName ++ testsFileExt
  doesFileExist file >>= \case
    True -> return file
    False -> do
      files <- getTestFiles dirPath <&> filter (isPrefixOf testsName)
      case files of
        [] -> error $ unwords ["file", testsName, "doesn't exist"]
        [x] -> return $ dirPath ++ x
        _ -> error $ unwords ["many files", files & intercalate ", "]

getTestFiles dir = do
  files <- getDirectoryContents dir
  return (files & filter (not . isPrefixOf ".") & filter (isSuffixOf testsFileExt))

createTestFileTxt :: [(String, String)] -> String
createTestFileTxt list = list &map (\(input, output) -> [input, "-", output] & intercalate "\n") &intercalate "\n=\n"

writeTestFile fileName list = writeFile fileName (createTestFileTxt list)

refreshFile (d,f) testsName = do
  file <- findTestSetFilePath d testsName
  (_, tests) <- readTestSet file
  let results = tests & map (newTestSetText f . snd)
  writeTestFile file results

refreshDir m@(d,f) = do
  let dir = root ++ d
  files <- getTestFiles dir 
  traverse (refreshFile m) (files <&> Fp.dropExtension)
