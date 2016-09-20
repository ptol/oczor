module Oczor.Test.TestCompiler where

import Test.Hspec
import Oczor.Syntax.Syntax
import ClassyPrelude
import Oczor.Compiler.Compiler
import qualified System.IO.Strict as S
import Oczor.Compiler.State
import Oczor.Utl

libs = "libs/"
root = "tests/compiler/"
output = root ++ "output/"

tests = ["code", "prelude", "import", "numbers", "syntax", "maybe", "foldable", "array", "either", "list"] <&> ("test_" ++)

tests2 = ["code", "prelude", "numbers", "syntax", "maybe"] <&> ("test_" ++)

std = ["prelude", "foldable", "maybes", "eithers", "lists"] <&> ("std." ++)

files = ["testModule", "testing"] ++ std ++ tests

files2 = do {mdl <- tests2; lang <- ["lua", "rb", "el"]; return (lang, mdl)}
  
state lng comb = initState
  & outputDir .~ output
  & srcDirs .~ [root, libs]
  & combine .~ comb
  & lang .~ lng

compileModulesJs :: [ModuleName] -> IO (Either Error (Map ModuleName String))
compileModulesJs names = runCompiler (state "js" False) $ do
  names & traverse_ loadModule 
  use modulesLangSrc


compileSrcJsModule :: ModuleName -> IO (Either Error String)
compileSrcJsModule name = runCompiler (state "js" False) (compileSrc name)

compilem name = runCompilerPrint (state "js" False) (compileAndWrite $ fileToModuleName name)

compileLang lang file = runCompilerPrint (state lang True) $ compileAndWrite (fileToModuleName file)

compile = compileLang "js"
compileLua = compileLang "lua"
compileRb = compileLang "rb"
compileEl = compileLang "el"

browse file = runCompilerPrint (state "js" False & showModule .~ True) $ compileAndWrite (fileToModuleName file)

readLangOutSimple outputDir x = S.readFile (langOutputFilePath outputDir "js" False x)

refreshTests2 = traverse_ (uncurry compileLang) files2

refreshTests = traverse_ compile tests

refreshFiles = traverse_ compilem files

file :: String -> IO (SpecWith ())
file file = do
  let x = fileToModuleName file
  input <- compileSrcJsModule x
  output <- readLangOutSimple output x
  return $ it (moduleNameToIdent x) $ input `shouldBe` Right output

checkFiles :: [String] -> IO ()
checkFiles x = do
  l <- traverse file x
  hspec $ describe "js" (sequence_ l)

f = checkFiles files


-- ff = checkFilesFast files
-- fileFast :: Map ModuleName String -> String -> _
-- fileFast modulesJs file = do
--   let x = fileToModuleName file
--   let input = modulesJs & lookupEx x
--   output <- readLangOutSimple output x
--   return $ it file $ do
--     input `shouldBe` output

-- checkFilesFast :: [String] -> IO ()
-- checkFilesFast x = do
--   modulesJs <- either (error . show) id <$> compileModulesJs (x <&> fileToModuleName)
--   l <- traverse (fileFast modulesJs) x
--   hspec $ describe "files" (sequence_ l)


-- spec = hspec $ parallel $ do
--   describe "expensiveOperation" $ do
--     replicateM_ 20 $ do
--       it "is expensive" $ do
--         sum [1..100000000] `shouldBe` 5000000050000000
