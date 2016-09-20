module Oczor.Test.Tests where

import Oczor.Parser.Parser
import Oczor.Syntax.Syntax
import ClassyPrelude
import Oczor.Test.TestEngine
import Oczor.Test.Files
import Oczor.Infer.Infer
import Oczor.Converter.Converter
import Oczor.Converter.Rewriter as Rewriter
import Oczor.CodeGen.CodeGenJs
import Oczor.Test.TestCompiler
import Control.Arrow
import Oczor.Utl
import Oczor.Compiler.Compiler
import Oczor.Pretty.Pretty

a = p >> i >> t >> c >> cc >> g

af = a >> f

mp = ("parser", \x -> (show +++ show) (removeMD <$> parseExpr x))
mi = ("infer", \x -> either (Right . show) Right $ (map show) (lastType <$> inferTxt x))
my = ("pretty", \x -> either (Right . show) Right $ (map prettyShow) (inferTxt x))
mt = ("inferast", \x -> (show +++ show) $ (removeContext . snd) <$> inferAllTxt x)
mc = ("converter", (show +++ show) . convertTxt2)
mcc = ("converter-class", \x -> either (Right . show) (Right . show)  $ compileJsPartTxt (pack x))
mr = ("rewriter", (show +++ (pshow . Rewriter.rewrite "js")) . convertTxt2)
mg = ("codegen", \x -> either (Right . show) (Right . show)  $ compileJsPartTxt (pack x))

p = checkDir mp
refreshp = refreshFile mp
refreshpDir = refreshDir mp

i = checkDir mi
refreshi = refreshFile mi
refreshiDir = refreshDir mi

y = checkDir my
refreshy = refreshFile my
refreshyDir = refreshDir my

t = checkDir mt
refresht = refreshFile mt
refreshtDir = refreshDir mt

c = checkDir mc
refreshc = refreshFile mc
refreshcDir = refreshDir mc

cc = checkDir mcc
refreshcc = refreshFile mcc
refreshccDir = refreshDir mcc

r = checkDir mr
refreshr = refreshFile mr
refreshrDir = refreshDir mr

g = checkDir mg
refreshg = refreshFile mg
refreshgDir = refreshDir mg

convertTxt2 x = do
  (context, tast) <- inferAllTxt x
  return $ convert2 context tast

convertTxt x = either (putStrLn . pack . show) (putStrLn . pack . pshow) (convertTxt2 x)
inferAstTxt x = either (putStrLn . pack . show) (putStrLn . pack . pshow) (inferAstTxt2 x)

codegenTxt2 x = do
  (context, tast) <- inferAllTxt x
  return $ codeGen $ convert2 context tast

codegenTxt x = either (putStrLn . pack . show) (putStrLn . pack . show) (codegenTxt2 x)
