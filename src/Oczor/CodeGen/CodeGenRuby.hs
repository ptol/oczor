module Oczor.CodeGen.CodeGenRuby where
import Oczor.CodeGen.Utl

codeGen :: Ast -> Doc
codeGen = x
  where
  x = cata $ \case
    NoneF -> empty
    UniqObjectF {} -> text "UniqObject.new"
    CodeF x -> text x
    NotEqualF x y -> sep [x, text "!=", y]
    EqualF x y -> sep [x, text "==", y]
    LitF value -> lit value
    IdentF i -> ident i
    VarF name ast -> hsep [ident name, equals, ast]
    SetF astl astr -> hsep [astl, equals, astr]
    ThrowF error -> hsep [text "raise", dquotes $ text error]
    IfF p l r -> hsep [text "if", p] <> nested l <$> (if onull r then end else text "else" <> nested r <$> end)
    ReturnF ast -> hsep [text "return", ast]
    FieldF ast name -> field ast name
    HasFieldF ast name -> sep [field ast name, text "!=", text "nil"]
    ObjectF list -> bracesNest $ punctuate comma (list <&> (\(name,ast) -> hsep [text ":" <> ident name, text "=>", ast]))
    FunctionF params body -> hcat [text "->", parens $ cat $ punctuate comma (params <&> text) ] <+> text "do" <> nested body <$> end
    ScopeF list y -> parens (func [] (list ++ [text "return" <+> y]) ) <> dot <> parens empty
    CallF name args -> name <> dot <> parens (hsep $ punctuate comma args)
    OperatorF name param -> (hsep $ case param of {[x] -> [text name, x]; [x,y] -> [x,text name, y]} )
    ArrayF list -> jsArray list
    ConditionOperatorF astb astl astr -> parens $ hsep [astb, text "?", astl, text ":", astr]
    BoolAndsF list -> parens $ cat $ punctuate (text " && ") list
    StmtListF list -> vcat list
    ParensF x -> parens x

func params body = hcat [text "->", parens $ cat $ punctuate comma (params <&> text) ] <+> text "do" <> nested body <$> end

lit = createLit (('?':) . (:[])) "nil" ("true", "false")

keywords = ["__ENCODING__", "def", "in", "self", "__LINE__", "defined?", "module", "super", "__FILE__", "do", "next", "then", "BEGIN", "else", "nil", "true", "END", "elsif", "not", "undef", "alias", "end", "or", "unless", "and", "ensure", "redo", "until", "begin", "false", "rescue", "when", "break", "for", "retry", "while", "case", "if", "return", "yield", "class"]

ident = createIdent keywords

field ast name = ast <> brackets (text ":" <> ident name)

nested l = nest 2 (empty <$> vcat l)

bracesNest l = if onull l then braces empty else nest 2 (lbrace <$> vcat l) <$> rbrace

end = text "end"
