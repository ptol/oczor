module Oczor.Converter.Rewriter where
import Oczor.Utl
import ClassyPrelude
import Oczor.Converter.CodeGenAst

ffSymbols :: Map String String
ffSymbols = mapFromList [
  ("andBool", "&&"),
  ("orBool", "||"),
  ("not", "!")
  ]

ffNames :: Map String String
ffNames = mapFromList [
  ("andBool", "and"),
  ("orBool", "or"),
  ("not", "not")
  ]

ffRules :: Map String (Map String String)
ffRules = (["js", "rb"] <&> (,ffSymbols)) ++ (["lua", "el"] <&> (,ffNames)) & mapFromList

commonInstancRules = [
  (("Int", "add"), "+"),
  (("Int", "mul"), "*"),
  (("Int", "sub"), "-"),
  (("Double", "add"), "+"),
  (("Double", "mul"), "*"),
  (("Double", "sub"), "-"),
  (("Double", "div"), "/")
  ]
intDivRule = [(("Int", "div"), "/")]

instanceSymbols :: String -> Bool -> Map (String, String) String
instanceSymbols eqOp hasIntDiv = mapFromList $ commonInstancRules ++ (bool [] intDivRule hasIntDiv) ++ (allTypes <&> (\x -> ((x,"eq"), eqOp))) where
  allTypes = ["Int", "Double", "Char", "Bool", "String"]

instanceNames :: Map (String, String) String
instanceNames = mapFromList $ commonInstancRules ++ intDivRule ++ [
  (("Int", "eq"), "eql"),
  (("Double", "eq"), "eql"),
  (("Bool", "eq"), "eql"),
  (("Char", "eq"), "char-equal"),
  (("String", "eq"), "string=")
  ]

instanceRules :: Map String (Map (String, String) String)
instanceRules = [
  ("js", instanceSymbols "===" False),
  ("lua", instanceSymbols "==" False),
  ("rb", instanceSymbols "==" True),
  ("el", instanceNames)] & mapFromList

rewrite :: String -> Ast -> Ast
rewrite lang = cata $ \case
  -- x | traceArgs ["rewrite", show x] -> undefined
  FunctionF params [Return (Call expr args)] | (params <&> Ident) == args -> expr -- eta reduction
  CallF (Field (Field (Field (Ident "oc") "instances") func) tp) param
    | Just operator <- instanceRules & lookup lang >>= lookup (tp, func) -> Parens $ Operator operator param
  CallF (Field (Field (Field (Ident "oc") "std") "ff") x) param | Just operator <- ffRules & lookup lang >>= lookup x -> Parens $ Operator operator param
  CallF x@Function {} y -> Call (Parens x) y
  x -> Fix x
