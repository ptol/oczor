module Oczor.CodeGen.CodeGenJs where
import Oczor.CodeGen.Utl

codeGen :: Ast -> Doc
codeGen = x
  where
  x = cata $ \case
    NoneF -> empty
    UniqObjectF {} -> text "{}"
    CodeF x -> text x
    NotEqualF x y -> sep [x, text "!=", y]
    EqualF x y -> sep [x, text "==", y]
    LitF value -> lit value
    IdentF name -> ident name
    VarF name ast -> stmt [text "var", text name, equals, ast]
    SetF astl astr -> stmt [astl, equals, astr]
    ThrowF error -> stmt [text "throw", dquotes $ text error]
    IfF p l r -> hcat [text "if", parens p] <> bracesNest l <> if onull r then empty else text "else" <> bracesNest r
    ReturnF ast -> stmt [text "return", ast]
    FieldF ast name -> field ast name
    HasFieldF ast name -> sep [field ast name, text "!==", text "undefined"]
    ObjectF list -> bracesNest $ punctuate comma (list <&> (\(name,ast) -> hsep [ident name, text ":", ast]))
    FunctionF params body -> func params body
    ScopeF list y -> parens (func [] (list ++ [text "return" <+> y]) ) <> parens empty
    CallF name args -> name <> parens (hcat $ punctuate comma args)
    OperatorF name param -> (hsep $ case param of {[x] -> [text name, x]; [x,y] -> [x,text name, y]} )
    ArrayF list -> jsArray list
    ConditionOperatorF astb astl astr -> parens $ hsep [astb, text "?", astl, text ":", astr]
    BoolAndsF list -> parens $ hcat $ punctuate (text " && ") list
    StmtListF list -> vcat list
    ParensF x -> parens x
    LabelF x y -> stmt [text "var", text x, equals, y]

func params body = hcat [text "function", parens $ hcat $ punctuate comma (params <&> text) ] <> bracesNest body

lit = createLit show "null" ("true", "false")

keywords = setFromList ["false", "true", "null", "abstract", "arguments", "boolean", "break", "byte case", "catch", "char", "class", "const continue", "debugger", "default", "delete", "do double", "else", "enum", "eval", "export extends", "false", "final", "finally", "float", "for", "function", "goto", "if", "implements import", "in", "instanceof", "int", "interface let", "long", "native", "new", "null package", "private", "protected", "public", "return short", "static", "super", "switch", "synchronized this", "throw", "throws", "transient", "true try", "typeof", "var", "void", "volatile while", "with", "yield"]

ident = createIdent keywords

stmt x = hsep x <> text ";"

field ast name = ast <> dot <> ident name


