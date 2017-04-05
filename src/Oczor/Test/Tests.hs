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

mp = ("parser", (show +++ show) . fmap removeMD . parseExpr)
mi = ("infer", Right . either show id . map show . fmap lastType . inferTxt)
my = ("pretty", Right . either show id . map prettyShow . inferTxt)
mt = ("inferast", (show +++ show) . fmap (removeContext . snd) . inferAllTxt)
mc = ("converter", (show +++ show) . convertTxt2)
mcc = ("converter-class", Right . either show show . compileJsPartTxt . pack)
mr = ("rewriter", (show +++ (pshow . Rewriter.rewrite "js")) . convertTxt2)
mg = ("codegen", Right . either show show . compileJsPartTxt . pack)

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

convertTxt2 = inferAllTxt >=> return . uncurry convert2

convertTxt = putStrLn . pack . either show  pshow . convertTxt2
inferAstTxt = putStrLn . pack . either show pshow . inferAstTxt2

codegenTxt2 x = do
  (context, tast) <- inferAllTxt x
  return $ codeGen $ convert2 context tast

codegenTxt, convertTxt, inferAstTxt :: String -> IO ()
codegenTxt = putStrLn . pack . either show show . codegenTxt2
