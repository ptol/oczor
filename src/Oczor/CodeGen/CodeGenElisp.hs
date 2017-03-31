module Oczor.CodeGen.CodeGenElisp (codeGen) where
import Oczor.CodeGen.Utl
import qualified Prelude as P
import Data.List.Split

codeGen :: Ast -> Doc
codeGen = code where
  code = \case
    None -> empty
    UniqObject name -> parens (text "lambda")
    NotEqual x y -> c "not" [c "eq" [code x, code y]]
    Equal x y -> c "eq" [code x, code y]
    Lit value -> lit value
    Ident name -> ident name
    Label x y -> p [text x, code y]
    Var name ast -> c "setq" [ident name, code ast]
    Set (Field ast name) astr -> c "puthash" [symbol name, code astr, code ast]
    Set astl astr -> c "setq" [code astl, code astr]
    Throw error -> c "error" [dquotes $ text error]
    If p l r -> cn "if" [code p, progn (l <&> code), progn (r <&> code)]
    Return ast -> code ast
    Field ast name -> field (code ast) name
    HasField ast name -> c "gethash" [symbol name, code ast]
    Object list -> c "oc-hash-from-alist" [if onull list then text "'()" else cn "list" (list &map (\(name,ast) -> symbol name <+> code ast))]
    Function params body -> func params body
    Scope list y -> scope list y
    Call name args -> c "funcall"((name : args) <&> code)
    Operator name x -> p (text name : (x <&> code) )
    Array list -> brackets (sep (list <&> code))
    ConditionOperator astb astl astr -> c "if" [code astb,code astl,code astr]
    BoolAnds list -> c "and" (list <&> code)
    StmtList list -> progn (list <&> code)
    Parens x -> code x
    x -> error $ unwords ["codegen", show x]

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

parensNest l = if onull l then parens empty else nest 2 (lparen <$> vcat l) <$> rparen

symbol s = text "'" <> ident s

field ast name = p [text "gethash", symbol name, ast]

p l = parens (hsep l)
c name args = p (text name : args)
cn name args = nest 2 ((lparen <> text name) <$> vcat args) <> rparen

progn = \case
  [] -> empty
  [x] -> x
  l -> cn "progn" l
