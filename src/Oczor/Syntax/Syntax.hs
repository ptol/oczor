module Oczor.Syntax.Syntax (module Oczor.Syntax.Syntax, module X) where
import ClassyPrelude hiding (TVar)
import qualified Data.Map as Map
import Oczor.Utl
import Data.Monoid
import Oczor.Syntax.Types as X
import Oczor.Syntax.Ast as X
import Oczor.Syntax.Errors as X
import Oczor.Syntax.Operators as X
import qualified Control.Monad.Writer.Strict as W

import Data.Functor.Foldable

sysPrefix = "_"


isTVar = \case
  TypeVar {} -> True
  _ -> False

lastType = \case
  (TypeRecord list) | Just l <- lastMay list -> l
  x -> x

constraintSetToMap :: ConstraintSet -> Map String [String]
constraintSetToMap set = set & groupMapBy fst & map (\(TypeVar x,y) -> (x,map snd y)) & mapFromList

collectConstrainFromTypeExpr :: TypeExpr -> ConstraintSet
collectConstrainFromTypeExpr x = removeAllConstraints x & snd

removeAllConstraints :: TypeExpr -> (TypeExpr, ConstraintSet)
removeAllConstraints x = cataM alg x & W.runWriter & (_2 %~ ordNub) where
  alg = \case
    TypeConstraintsF list x -> do
      W.tell list
      return x
    x -> return $ embed x

moveConstraintsOnTop x =
  if onull set then x
  else TypeConstraints set ast
  where
    (ast, set) = removeAllConstraints x

renameTypeVarsInConstraints m = map (\(x,l) -> (Map.findWithDefault x x m, l))

renameTypeVars :: Map String String -> TypeExpr -> TypeExpr
renameTypeVars f = cata $ \case
  (TypeVarF x) -> TypeVar $ Map.findWithDefault x x f
  -- (TypeConstraintsF c y) -> TypeConstraints (renameTypeVarsInConstraints f c) y
  x -> embed x


exprAppend (Record l1) (Record l2) = Record (l1 ++ l2)
exprAppend x (Record l) = Record (x : l)
exprAppend (Record l) x = Record (l ++ [x])
exprAppend x y = Record [x,y]

isIdent (Ident _) = True
isIdent _ = False

isRecordField RecordLabel {} = True
isRecordField _ = False

isTypeIdent (TypeIdent _) = True
isTypeIdent _ = False

isTypeUnion (TypeUnion _) = True
isTypeUnion _ = False

isTypeLabel TypeLabel {} = True
isTypeLabel _ = False

unwrapMD = \case
  (MD x y) -> y
  x -> x

newMD p = \case
  x@MD{} -> x
  y -> MD p y

recordToList = unwrapMD >>> \case
  Record list -> list
  ExprList list -> list
  x -> [x]
expandExprLists = \case
  (ExprListMD x) -> x
  x -> [x]

recordIfSome :: [Expr] -> Expr
recordIfSome l = case l >>= expandExprLists of {[x] -> x; x -> Record x}

recordIfSomeExceptRecord :: [Expr] -> Expr
recordIfSomeExceptRecord = \case
  x@[Record{}] -> Record x
  x -> recordIfSome x

listIfSome :: [Expr] -> Expr
listIfSome = \case {[x] -> x; x -> ExprList x}

typeUnionIfSome :: [TypeExpr] -> TypeExpr
typeUnionIfSome = \case {[x] -> x; x -> TypeUnion x}

exprListToRecord (MD md x) = newMD md (exprListToRecord x)
exprListToRecord (ExprList x) = recordIfSome x
exprListToRecord x = x

listToLetOrRecord = \case
  -- x | traceArgs ["listToletOrRecord", show x] -> undefined
  [] -> error "listToLet []"
  [x] -> x
  list ->
    case l of
      (MD md (In _ x)) -> if onull i then x else MD md $ Let (recordIfSome i) x
      _ -> recordIfSome list
    where
      (i,l) = unsafeUnconsLast list

isTupleLabel = isPrefixOf "item"

isRecordTupleLabel (RecordLabel label _) | isTupleLabel label = True
isRecordTupleLabel _ = False

getTupleLabelType = \case
  TypeLabel lbl tp | isTupleLabel lbl -> Just tp
  _ -> Nothing


-- getTypeIdent x | traceArgs ["getTypeIdent", show x] = undefined
getTypeIdent (TypeIdent x) = Just x
getTypeIdent (TypeApply (TypeIdent x) _) = Just x
getTypeIdent _ = Nothing

typeExprToList (TypeRecord l) = l
typeExprToList x = [x]

getTypeLabels :: TypeExpr -> [String]
getTypeLabels (TypeRow x y) = (y ++ [x]) >>= getTypeLabels
getTypeLabels (TypeRecord x) = x >>= getTypeLabels
getTypeLabels (TypeLabel x _) = [x]
getTypeLabels _ = []

hasTypeVar :: TypeExpr -> Bool
hasTypeVar = getAny . cata (\case
      TypeVarF _ -> Any True
      x -> ffold x)

instanceTypeName = \case
  (TypeIdent x) -> x
  (TypeConstraints _ x) -> instanceTypeName x
  (TypeApply x _) -> instanceTypeName x
  x -> error $ "instanceTypeName " ++ show x

getTypeVars :: TypeExpr -> [String]
getTypeVars = cata $ \case
  TypeVarF x -> [x]
  x -> ffold x

curryTypeApply2 arg@(TypeApply x arga) param@(TypeApply y argp) =
  if olength arga < olength argp then
    (arg, curryTypeApply (olength arga) param)
  else
    (curryTypeApply (olength argp) arg, param)

curryTypeApply arity ast@(TypeApply x params) =
  let l = olength params in
    if arity > l then error $ unwords ["curryTypeApply", show ast, " arity > l", show arity]
    else if arity == l then ast
    else let (sl,sr) = splitAt arity params in TypeApply (TypeApply x sl) sr

newTypeRecord :: [TypeExpr] -> TypeExpr
newTypeRecord = \case {[x] -> x; x -> TypeRecord x}

removeMD :: Expr -> Expr
removeMD = cata $ \case
  MDF _ x -> x
  x -> embed x

funcArity = \case
  Function (Record list) _ _ -> olength list
  Function {} -> 1

typeArity = \case
  TypeRecord list -> olength list
  _ -> 1

casesArity list = list & map funcArity & ordNub

getLabelType label = \case
  TypeLabel lbl tp | lbl == label -> Just tp
  TypeRecord list -> list <&> getLabelType label & catMaybes & headMay
  TypeConstraints c x -> getLabelType label x <&> TypeConstraints c -- TODO filter constraints in c
  TypeRow x y -> getLabelType label (TypeRecord y)
  _ -> Nothing

moduleNameToIdent :: [String] -> String
moduleNameToIdent = intercalate "."
