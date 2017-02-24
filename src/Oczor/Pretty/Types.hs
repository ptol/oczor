module Oczor.Pretty.Types where
import Text.PrettyPrint.Leijen as PP
import ClassyPrelude hiding ((<>), empty, (<$>))
import Oczor.Syntax.Syntax
import Oczor.Infer.Infer
import Oczor.Utl hiding ((<+>))

commaSep list  = hcat (punctuate (text ", ") list)

instance Pretty TypeExpr where
  pretty x = cata alg x 0 where
    alg ast parentPrec = par $ case ast of
      (TypeIdentF name) -> text name
      (TypeVarF name) -> text name
      (TypeLabelF name eType) -> hsep [text name, char ':', eType prec]
      (TypeRecordF list) -> commaSep (list <&> ($ prec) & filter (\x -> show x /= ""))
      (TypeRowF x list) -> x prec <> text "@" <> commaSep (list <&> ($ prec))
      (TypeUnionF list) -> hcat (punctuate (text " | ") (list <&> ($ prec)))
      (TypeFuncF inType outType) -> hsep [inType prec, text "=>", outType prec]
      (TypeApplyF expr arg) -> hsep $ expr prec : (arg <&> ($ prec))
      (TypeConstraintsF list expr) -> hsep [commaSep (list &map (\(x,y) -> x prec <+> text y)), text "<:", expr prec ]
      (TypePolyF x y ) -> y prec
      NoTypeF -> empty
      where
        prec = getPrec ast
        par = if prec < parentPrec then parens else id

getPrec = \case
  TypeFuncF {} -> 50
  TypeRecordF {} -> 60
  TypeUnionF {} -> 70
  TypeLabelF {} -> 80
  TypeConstraintsF {} -> 90
  _ -> 100


moduleIdents mdl = mdl ^. idents & mapToList <&> (\(ident, Forall _ tp) -> hsep [text ident, char ':', pretty tp]) & vcat
  
