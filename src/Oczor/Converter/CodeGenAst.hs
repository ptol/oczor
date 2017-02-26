{-# LANGUAGE PatternSynonyms       #-}
module Oczor.Converter.CodeGenAst (module Oczor.Converter.CodeGenAst, module Oczor.Converter.CodeGenAstF) where
import Data.Functor.Foldable hiding (Foldable)
import ClassyPrelude
import Oczor.Converter.CodeGenAstF
import Oczor.Utl

type Ast = Fix AstF

pattern None = Fix NoneF
pattern Lit x = Fix (LitF x)
pattern UniqObject x = Fix (UniqObjectF x)
pattern Ident x = Fix (IdentF x)
pattern Var x y = Fix (VarF x y)
pattern Set x y = Fix (SetF x y)
pattern Throw x = Fix (ThrowF x)
pattern StmtList x = Fix (StmtListF x)
pattern BoolAnds x = Fix (BoolAndsF x)
pattern Array x = Fix (ArrayF x)
pattern Field x y = Fix (FieldF x y)
pattern HasField x y = Fix (HasFieldF x y)
pattern ConditionOperator x y z = Fix (ConditionOperatorF x y z)
pattern Call x y = Fix (CallF x y)
pattern Label x y = Fix (LabelF x y)
pattern Object x = Fix (ObjectF x)
pattern Scope x y = Fix (ScopeF x y)
pattern Function x y = Fix (FunctionF x y)
pattern Return x = Fix (ReturnF x)
pattern Operator x y = Fix (OperatorF x y)
pattern If x y z = Fix (IfF x y z)
pattern NotEqual x y = Fix (NotEqualF x y)
pattern Equal x y = Fix (EqualF x y)
pattern Parens x = Fix (ParensF x)
pattern Code x = Fix (CodeF x)


scopeToFunc (ScopeF x y) = if onull x then y else CallF (Parens (Function [] (x ++ [ReturnF (Fix y)] <&> Fix))) []
  
-- pattern Scope x <- Function _ x

getVarName (Var x _) = Just x
getVarName _ = Nothing

isFunction Function{} = True
isFunction _ = False

astToList (StmtList x) = x
astToList x = [x]

litString x = Lit $ LitString x

setField obj label expr = Set (Field obj label) expr

emptyObject = Object []

containsIdents :: [String] -> Ast -> [String]
containsIdents list = cata $ \case
  IdentF x | oelem x list -> [x]
  x -> ffold x
