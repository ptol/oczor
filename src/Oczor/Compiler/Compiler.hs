module Oczor.Compiler.Compiler(module Oczor.Compiler.Compiler, module X) where

import ClassyPrelude as C
import Oczor.Infer.InferContext
import Control.Monad.State
import Control.Monad.Except
import Oczor.Syntax.Syntax
import Oczor.Parser.Parser
import Oczor.Converter.Converter
import Oczor.Converter.Rewriter
import Oczor.Compiler.State
import Oczor.Compiler.Utl as X
import Oczor.Compiler.Files as X
import Oczor.Pretty.Pretty
import Oczor.Utl hiding (rewrite)

import qualified Oczor.CodeGen.CodeGenJs as Js
import qualified Oczor.CodeGen.CodeGenRuby as Ruby
import qualified Oczor.CodeGen.CodeGenElisp as Elisp
import qualified Oczor.CodeGen.CodeGenLua as Lua

langs :: Map _ _
langs = [
  ("js", Js.codeGen),
  ("rb", Ruby.codeGen),
  ("el", Elisp.codeGen),
  ("lua", Lua.codeGen)] & mapFromList

preFile :: ModuleName -> String -> Compiler String
preFile name langSrc = do
  comb <- use combine
  if comb then do
    x <- readPreMay name
    return $ maybe langSrc (\pre -> joinLines [pre,langSrc]) x
  else return langSrc

compileModule :: ModuleName -> OcWithFfi -> Compiler ()
compileModule name (ffiCode, oc) = do
  mdls <- use loadModules
  lng <- use lang
  let ctx = emptyContext & modules .~ mdls
  (context, ast) <- liftE $ inferAllTxtWith ctx name oc
  let tast = rewrite lng $ convertModule context ast name ffiCode
  langSrc <- (langs & lookupEx lng) tast & show & preFile name
  loadModules %= insertMap name (context ^. cmodule)
  modulesOrder %= (name :)
  modulesLangSrc %= insertMap name langSrc
  return ()

changeErrorPosition :: ModuleName -> Error -> Compiler ()
-- changeErrorPosition name | traceArgs ["changeErrorPosition", show name] = undefined
changeErrorPosition name = \case
  (x@(ModuleNotExists mn),(_,_,"")) -> throwError (x, (1,1,combinePath name ++ ocExt))
  x -> throwError x

compileModuleLoadImports :: ModuleName -> Compiler ()
compileModuleLoadImports name = do
  (ffi, oc) <- readWithFfi name
  imports <- liftE $ getImports oc
  catchError (imports & traverse_ loadModule) (changeErrorPosition name)
  compileModule name (ffi, oc)

loadModule :: ModuleName -> Compiler ()
-- loadModule name | traceArgs ["loadModule", show name] = undefined
loadModule n = do
  name <- fixModuleNameIfDir n
  compModules <- use compilingModules
  if elem name compModules then filePathOc name >>= (\x -> throwError (CircularDependency compModules, (1,1,x))) 
  else do
    compilingModules %= (name :)
    mdl <- use loadModules <&> lookup name
    mdl & maybe (compileModuleLoadImports name) (const $ return ())
    compilingModules %= filter (/= name)

runCompiler :: CompState -> Compiler a -> IO (Either Error a)
runCompiler st m = runExceptT $ evalStateT m st

runCompilerPrint :: CompState -> Compiler a -> IO ()
runCompilerPrint st m = do
  r <- runCompiler st m
  either printError (const $ putStrLn $ pack "compilation is completed") r

printError (tp, (row, col, file)) = do
  let text = file ++ ":" ++ show row ++ ":" ++ show col
  putStrLn $ pack text
  putStrLn $ pack $ prettyShow tp

compileSrc name = do
  loadModule name
  lng <- use lang
  st <- get
  if st ^. combine then 
    return $ st ^. modulesOrder & reverse <&> (\x -> st ^. modulesLangSrc & lookupEx x) & unlines
  else
    return $ st ^. modulesLangSrc & lookupEx name

compileAndWrite :: ModuleName -> Compiler ()
compileAndWrite name = do
  langSrc <- compileSrc name
  showMdl <- use showModule
  if showMdl then do
    Just mdl <- use loadModules <&> lookup name
    putStrLn $ pack $ prettyShow $ moduleIdents mdl
  else do
    outPath <- filePathLangOut name
    io $ writeFileCreateDir outPath langSrc

