module Oczor.Compiler.Files where

import ClassyPrelude as C
import Control.Monad.Except
import Oczor.Syntax.Syntax
import qualified System.FilePath as Fp
import qualified System.IO.Strict as S
import System.Directory
import Oczor.Compiler.State
import Oczor.Infer.Module
import Oczor.Compiler.Utl
import Oczor.Utl hiding (rewrite)

read x = io $ S.readFile x

ocExt = ".oc"

combinePath :: [String] -> String
combinePath = intercalate "/"


fixModuleNameIfDir moduleName = do
  let rootName = rootModuleName moduleName
  let fileName = combinePath rootName ++ ocExt
  maybe moduleName (const rootName) <$> findFilePathInDirs fileName

findFilePathInDirs fileName = do
  dirs <- use srcDirs
  cd <- io getCurrentDirectory
  let paths =  fileName : (dirs <&> (++ fileName))
  headMay <$> io (C.filterM doesFileExist paths)

filePathOc :: ModuleName -> Compiler FilePath
filePathOc moduleName = do
  let fileName = combinePath moduleName ++ ocExt
  path <- findFilePathInDirs fileName
  maybe (throwError (ModuleNotExists moduleName, (1,1,""))) return path

ocPathToLangPath :: FilePath -> Compiler FilePath
ocPathToLangPath ocPath = Fp.replaceExtension ocPath <$> use lang

ocPathToPrePath :: FilePath -> Compiler FilePath
ocPathToPrePath ocPath = Fp.replaceExtension ocPath <$> (use lang <&> ("pre." ++))

filePathLangOut :: ModuleName -> Compiler FilePath
filePathLangOut x = langOutputFilePath <$> use outputDir <*> use lang <*> use combine <*> pure x

langOutputFilePath outputDir lang combine x =
  Fp.combine outputDir $ combinePath x ++ bool ".module" "" combine ++ "." ++ lang

readFileMay file = do
  exists <- io $ doesFileExist file
  if exists then Just <$> read file
  else return Nothing

readWithFfi :: ModuleName -> Compiler OcWithFfi
readWithFfi file = liftA2 (,) (readLangMay file) (readOc file)

readOc = filePathOc >=> read

readLangMay = filePathOc >=> ocPathToLangPath >=> readFileMay

readPreMay = filePathOc >=> ocPathToPrePath >=> readFileMay


readLangOut = filePathLangOut >=> read
