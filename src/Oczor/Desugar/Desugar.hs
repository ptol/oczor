module Oczor.Desugar.Desugar where

import ClassyPrelude 
import Oczor.Syntax.Syntax

import Control.Monad.State
import Oczor.Parser.ParserState
import Oczor.Utl
import qualified Control.Monad.Writer.Strict as W

import Data.Functor.Foldable

paramToBody newParam ast = case removeMD ast of
  Lit _ -> (Just . Call (Ident "eq") $ Record [Ident newParam, ast], Nothing)
  Ident x -> (Just . Call (Ident "eq") $ Record [Ident newParam, ast], Nothing)
  WildCard -> (Nothing, Nothing)
  RecordLabel x lit@(Lit _) ->
    (Just . Call (Ident "eq") $ Record [LabelAccessCall x $ Ident newParam, lit], Nothing)
  RecordLabel x (ParamIdent param) -> (Nothing, Just . RecordLabel param $ LabelAccessCall x (Ident newParam))
  RecordLabel x (Ident param) -> (Nothing, Just$ RecordLabel param (LabelAccessCall x (Ident newParam)))
  x -> error $ unwords ["paramToBody", show x]

astToLabel index = \case
  ast@RecordLabel {} -> ast
  ast -> RecordLabel ("item" ++ show index) ast

desugarFuncParam :: Expr -> Parser (Expr, [Expr], [Expr])
-- desugarFuncParam x | traceArgs ["desugarFuncParam", show x] = undefined
desugarFuncParam = \case
  (MD md x) -> do
    (nx,y,z) <- desugarFuncParam x
    return (newMD md nx, y, z)
  (Record list) -> do
    p <- asNameOrFresh
    let (g,b) = list &zip [1..] &map (\(i,x) -> (paramToBody p (astToLabel i x))) & unzip
    return (ParamIdent p, catMaybes g, catMaybes b)
  x@ParamIdent {} -> return (x, [], [])
  x@ExprType {} -> return (x, [], [])
  (As name ast) -> do
    asName .= name
    desugarFuncParam ast

  ast -> do
    p <- asNameOrFresh
    let (g,b) = paramToBody p ast
    return (ParamIdent p, maybeToList g, maybeToList b)

funcParam :: Expr -> Parser (Expr, [Expr], [Expr])
funcParam (MD md x) = do
  (nx,y,z) <- funcParam x
  return (newMD md nx, y, z)
funcParam (Record list) = do
  list <- traverse desugarFuncParam list
  let (x, guard, body) = unzip3 list
  return (recordIfSome x, concat guard, concat body)
funcParam x = funcParam (Record [x])

addToExpr x (Record list) = Record $ x ++ list
addToExpr x y = Record $ x ++ [y]
addToBody [] x = x
addToBody x (Let y r) = Let (addToExpr x y) r
addToBody x y = Let (recordIfSome x) y

addToGuard x = go . (x ++) . maybeToList
  where
    go [] = Nothing
    go [x] = Just x
    go (h : t) = Just $ foldl' (\a e -> Call (Ident "andBool") (Record [a,e])) h t

func (Function param guard body) = do
  cleanAsName
  (newParam, addGuard, addBody) <- funcParam (removeMD param)
  return $ Function newParam (addToGuard addGuard guard) (addToBody addBody body)

funcSingleParam (Function param guard body) = do
  cleanAsName
  (newParam, addGuard, addBody) <- desugarFuncParam param
  let newGuard = addToGuard addGuard guard
  let newGuard2 = if onull addBody then newGuard else Let (recordIfSome addBody) <$> newGuard
  return $ Function newParam newGuard2 (addToBody addBody body)

destruct (Destruct x y) = do
  cleanAsName
  (ParamIdent label, _, addBody) <- desugarFuncParam x
  return $ listIfSome (RecordLabel label y : addBody)

generateEmptyType name = let typeName = toTitleCase name in [
  FfiType typeName (TypeIdent typeName),
  RecordLabel name (WithType (UniqObject name) (TypeIdent typeName)),
  -- Ffi name (TypeIdent typeName),
  InstanceFn (TypeIdent typeName) "eq" (Ident "eqAny")
  ]


typeDecl :: String -> [TypeExpr] -> TypeExpr -> Expr
-- typeDecl name param body | traceArgs ["typeDeclBody", show param, show body] = undefined
typeDecl name param body = listIfSome $ (types >>= generateEmptyType) ++ [td] where
  params :: [String]
  params = param <&> (\(TypeVar x) -> x)
  alg :: TypeExprF TypeExpr -> W.Writer [String] TypeExpr
  alg (TypeVarF x) | x `notElem` params = do
    W.tell [x]
    return $ TypeIdent (toTitleCase x)
  alg x = return $ embed x
  (r,types) = W.runWriter $ cataM alg body
  td = TypeDecl name $ if onull param then r else TypePoly param r

partialApply ast = case ast of
  Call expr arg -> convert arg (Call expr) 
  Array arg -> convert (ExprList arg) (\(ExprList list) -> Array list)
  where
    convert :: Expr -> _
    convert arg createAst  = do
      (newArg, params) <- runStateT (cataM alg arg) []
      return $ if onull params then ast else Function (recordIfSome $ params <&> ParamIdent) Nothing (createAst newArg)
    alg :: ExprF Expr -> StateT [String] Parser Expr
    alg = \case
      WildCardF -> do
        fn <- lift freshName
        modify (++ [fn])
        return $ Ident fn
      x -> return $ embed x
