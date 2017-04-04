module Oczor.CodeGen.Utl (module Oczor.CodeGen.Utl, module X, (<&>), cata, (&), bracesNest) where
import Oczor.Converter.CodeGenAst as X
import ClassyPrelude as X hiding ((<>), empty, (<$>), (</>), bool, group)
import Text.PrettyPrint.Leijen as X
import Oczor.Utl 

createIdent :: Set String -> String -> Doc
createIdent keywords x = text $ if member x keywords then "_" ++ x else x

createLit charFunc null (t,f)  = \case
  LitInt value -> int value
  LitDouble value -> double value
  LitChar value -> text $ charFunc value
  LitString value -> text $ show value
  LitBool value -> text $ if value then t else f
  LitNull -> text null

jsArray = brackets . hcat . punctuate comma

bracesNest [] = braces empty
bracesNest l = nest 2 (lbrace <$> vcat l) <$> rbrace
