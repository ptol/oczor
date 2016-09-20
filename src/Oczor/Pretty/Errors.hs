module Oczor.Pretty.Errors where
import Text.PrettyPrint.Leijen as PP
import ClassyPrelude hiding ((<>), empty, (<$>))
import Oczor.Syntax.Syntax
import Oczor.Utl hiding ((<+>))
import Oczor.Pretty.Types

instance Pretty ErrorType where
  pretty = \case
    ParserError x -> text x
    UnificationFail x y -> hsep [text "cannot unify", pretty x, text "and", pretty y]
    InfiniteType x y -> hsep [text "infinite type", text x, pretty y]
    TextError x -> text x
    CircularDependency x -> hsep [text "circular dependency", commaSep (x <&> (text . moduleNameToIdent))]
    NoInstance t cls -> hsep [text "type", pretty t, text "doesn't have instance", text cls]
    UnboundVariable x -> hsep [text "unbound variable", text x]
    UnboundModule x -> hsep [text "unbound module", text $ moduleNameToIdent x]
    ModuleNotExists x -> hsep [text "module", text $ moduleNameToIdent x, text "doesn't exist"]
    UnboundType x -> hsep [text "unbound type", text x]
    UnboundClass x -> hsep [text "unbound class", text x]
    TypeUnionWithUnion x -> hsep [text "union type with union", text x]
    UnificationMismatch x y -> hsep [text "unification mismatch", commaSep (x <&> pretty), text "and", commaSep (x <&> pretty)]
    
