module Oczor.CodeGen.CodeGenElisp (codeGen) where
import Oczor.CodeGen.Utl
import qualified Prelude as P
import Data.List.Split
import Data.Functor.Foldable

phi = \case
  NoneF -> empty
  UniqObjectF name -> parens (text "lambda")
  NotEqualF x y -> c "not" [c "eq" [x, y]]
  EqualF x y -> c "eq" [x, y]
  LitF value -> lit value
  IdentF name -> ident name
  LabelF x y -> p [text x, y]
  VarF name ast -> c "setq" [ident name, ast]
  SetF astl astr -> c "setq" [astl, astr]
  ThrowF error -> c "error" [dquotes $ text error]
  IfF p l r -> cn "if" [p, progn l, progn r]
  ReturnF ast -> ast
  FieldF ast name -> field ast name
  HasFieldF ast name -> c "gethash" [symbol name, ast]
  ObjectF list -> c "oc-hash-from-alist" [if onull list then text "'()" else cn "list" (list &map (\(name,ast) -> symbol name <+> ast))]
  CallF name args -> c "funcall" (name : args)
  OperatorF name x -> p (text name : x)
  ArrayF list -> brackets (sep list)
  ConditionOperatorF astb astl astr -> c "if" [astb,astl,astr]
  BoolAndsF list -> c "and" list
  StmtListF list -> progn list
  ParensF x -> x
  x -> error $ unwords ["codegen", show x]

codeGen :: Ast -> Doc
codeGen = code where
  code = \case
    Set (Field ast name) astr -> c "puthash" [symbol name, code astr, code ast]
    Function params body -> func params body
    Scope list y -> scope list y
    x -> phi $ fmap codeGen $ project x

scope list r  = bodyCode
     where
       body = list ++ [Return r]
       (newBody, ffiVars) = fromMaybe (body, "") $ do
         h <- headMay body
         case h of {Code x -> Just (P.tail body, x); _ -> Nothing}
       vars = newBody <&> getVarName & catMaybes <&> (\x -> p [ident x, nil])
       bodyCode =
         if onull vars && ffiVars == "" then progn (newBody <&> codeGen)
         else cn "let*" $ parens (vcat (text ffiVars : vars)) : (newBody <&> codeGen)

func params body = p [text "lambda", p (params <&> ident), bodyCode ]
     where
       (newBody, ffiVars) = fromMaybe (body, "") $ do
         h <- headMay body
         case h of {Code x -> Just (P.tail body, x); _ -> Nothing}
       vars = newBody <&> getVarName & catMaybes <&> (\x -> p [ident x, nil])
       bodyCode =
         if onull vars && ffiVars == "" then progn (newBody <&> codeGen)
         else c "let*" [parensNest (text ffiVars : vars ), progn (newBody <&> codeGen)]

nil = text "nil"

lit = createLit (('?':) . (:[])) "nil" ("t", "nil")

convertName x = x & split (startsWithOneOf ['A'..'Z']) <&> toLower & intercalate "-"

keywords = setFromList ["not", "log", "eq"]
identKw = createIdent keywords
ident name = let newName = convertName name in identKw newName

parensNest [] = parens empty
parensNest l = nest 2 (lparen <$> vcat l) <$> rparen

symbol s = text "'" <> ident s

field ast name = p [text "gethash", symbol name, ast]

p l = parens (hsep l)
c name args = p (text name : args)
cn name args = nest 2 ((lparen <> text name) <$> vcat args) <> rparen

progn = \case
  [] -> empty
  [x] -> x
  l -> cn "progn" l
