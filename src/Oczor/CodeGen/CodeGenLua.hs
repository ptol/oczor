module Oczor.CodeGen.CodeGenLua where
import Oczor.CodeGen.Utl

codeGen :: Ast -> Doc
codeGen = x
  where
  x = cata $ \case
    NoneF -> empty
    UniqObjectF {} -> text "{}"
    CodeF x -> text x
    NotEqualF x y -> sep [x, text "~=", y]
    EqualF x y -> sep [x, text "==", y]
    LitF value -> lit value
    IdentF i -> ident i
    VarF name ast -> hsep [text "local", ident name, equals, ast]
    SetF astl astr -> hsep [astl, equals, astr]
    ThrowF error -> codeGen (Call (Ident "error") [litString error])
    IfF p l r -> hsep [text "if", p, text "then"] <> nested l <$> (if onull r then end else text "else" <> nested r <$> end)
    ScopeF list y -> parens (func [] (list ++ [text "return" <+> y]) ) <> parens empty
    ReturnF ast -> hsep [text "return", ast]
    FieldF ast name -> field ast name
    HasFieldF ast name -> sep [field ast name, text "~=", text "nil"]
    ObjectF list -> bracesNest $ punctuate comma (list <&> (\(name,ast) -> hsep [ident name, text "=", ast]))
    FunctionF params body -> hcat [text "function", parens $ cat $ punctuate comma (params <&> text) ] <> nested body <$> end
    CallF name args -> name <> parens (hsep $ punctuate comma args)
    OperatorF name param -> (hsep $ case param of {[x] -> [text name, x]; [x,y] -> [x,text name, y]} )
    ArrayF list -> braces (cat $ punctuate comma list)
    ConditionOperatorF astb astl astr -> parens $ hsep [astb, text "and", astl, text "or", astr]
    BoolAndsF list -> parens $ cat $ punctuate (text " and ") list
    StmtListF list -> vcat list
    ParensF x -> parens x

lit = createLit (show . (:[])) "nil" ("true", "false")

keywords = ["print", "and", "break", "do", "else", "elseif", "end", "false", "for", "function", "if", "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while"]

ident = createIdent keywords

field ast name = ast <> dot <> ident name

nested l = nest 2 (empty <$> vcat l)

bracesNest l = if onull l then braces empty else nest 2 (lbrace <$> vcat l) <$> rbrace

end = text "end"

func params body = hcat [text "function", parens $ cat $ punctuate comma (params <&> text) ] <> nested body <$> end
