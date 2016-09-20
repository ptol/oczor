module Oczor.Pretty.Pretty (
  module Oczor.Pretty.Pretty,
  module X) where
import Text.PrettyPrint.Leijen as PP
import ClassyPrelude 
import Oczor.Pretty.Types as X
import Oczor.Pretty.Errors as X

prettyShow x = show $ pretty x
