module Oczor.Compiler.Utl where

import Oczor.Infer.InferContext
import qualified Oczor.Parser.Parser as Parser
import Oczor.Pretty.Pretty
import Oczor.Syntax.Syntax
import Oczor.Utl

import ClassyPrelude
import Oczor.Infer.Infer
import Oczor.Converter.Converter
import qualified Oczor.CodeGen.CodeGenJs as Js
import qualified System.FilePath as Fp
import System.Directory
import Control.Monad.Except
import Oczor.Compiler.State
import Data.List.Split (splitOn)

replace old new = intercalate new . splitOn old

fileToModuleName = replace ".oc" "" >>> splitOn "."

liftE :: _ -> Compiler _
liftE = \case
  Right x -> return x
  Left x -> throwError x

io :: IO a -> Compiler a
io = liftIO

writeFileCreateDir path txt = do
  let dir = Fp.takeDirectory path
  createDirectoryIfMissing True dir
  writeFile path txt


inferAllTxtWith :: InferContext -> ModuleName -> String -> Either Error (InferContext, InferExpr)
inferAllTxtWith context fileName x = do
  (ast, ops) <- Parser.parseAll Parser.parser x fileName (context & allOperators)
  inferAllExpr (context & cmodule . operators .~ ops & cmodule . moduleName .~ fileName) ast

compileJsPartTxt x = do
  (context, tast) <- inferAllTxt x
  return $ Js.codeGen $ convert2 context tast

inferTxt2 x = either (putStrLn . pack . show) (putStrLn . pack . prettyShow) $ inferTxt x

inferType :: Expr -> Either Error TypeExpr
inferType y = (fst . attr . snd) <$> inferAllExpr baseTypeContext y

inferTxt :: String -> Either Error TypeExpr
inferTxt x = normalizeType <$> (Parser.parseExpr x >>= inferType)

inferAstTxt2 :: String -> Either Error InferExpr
inferAstTxt2 x = Parser.parseExpr x >>= (\y -> snd <$> inferAllExpr emptyContext y)
  
inferContext :: InferContext -> ModuleName -> String -> Either Error InferContext
inferContext context fileName x = fst <$> inferAllTxtWith context fileName x


inferAllTxt :: String -> Either Error (InferContext, InferExpr)
-- inferAllTxt x | traceArgs ["inferAllTxt", x] = undefined
inferAllTxt x = (Parser.parseExpr x >>= inferAllExpr baseTypeContext)

inferAllTxt2 x = do
  let Right (c, ast) = (Parser.parseExpr x >>= inferAllExpr emptyContext)
  putStrLn $ pack (unlines ["ast", show ast])
  return ""
