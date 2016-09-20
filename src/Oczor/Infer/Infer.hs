module Oczor.Infer.Infer (module Oczor.Infer.Infer, module X) where

import Oczor.Syntax.Syntax
import Oczor.Utl
import Oczor.Infer.Unify
import Control.Monad.RWS
import ClassyPrelude as C
import Oczor.Infer.InferContext as X
import Oczor.Infer.State
import Oczor.Infer.InferAst as X
import Oczor.Infer.Substitutable
import Control.Monad.Trans.Maybe

infer :: Expr -> Infer InferExpr
infer ast = {-trac ("inferResult " ++ show ast) <$>-} do
  ctx <- ask
  changeContext ctx <$> r

  where
  r = case ast of
    -- ast | traceArgs (["infer", show ast]) -> undefined
    MD pos x -> local (position .~ Just pos) $ infer x

    Stmt x -> return (annType (StmtF x) NoType)

    Ffi name expr -> return (annType (FfiF name expr) NoType)

    FfiType name expr -> return (annType (FfiTypeF name expr) NoType)

    ClassFn name tp -> return (annType (ClassFnF name tp) NoType)

    TypeDecl name expr -> return (annType (TypeDeclF name expr) NoType)

    InstanceFn tp name expr -> do
      ast <- infer expr
      let t = attrType ast
      context <- ask
      instanceType <- getInstanceType context name t
      match instanceType tp
      -- newType <- applySubst t -- TODO
      return (annType (InstanceFnF tp name ast) t)

    WithType expr tp -> do
      renamedTP <- renameVarsInType tp
      ast <- infer expr
      let t = attrType ast
      unifyWithSubst t renamedTP
      newType <- applySubst renamedTP
      return (changeType newType ast) -- TODO think WithType

    Lit x -> do
      let tp = inferLit x
      return (annType (LitF x) tp)

    UniqObject x -> do
      tp <- fresh
      return (annType (UniqObjectF x) tp)

    Ident x -> do
      tp <- lookupIdentType x
      return (annType (IdentF x) tp)

    RecordLabel name body -> do
      fv <- fresh
      newAst <- local (addIdentType name fv) (infer body)
      let tp = attrType newAst
      appFv <- applySubst fv
      unifyWithSubst appFv tp
      t <- applySubst $ TypeLabel name tp
      return (annType (RecordLabelF name newAst) t)

    Record x -> do
      (_, newAst) <- inferRecord ast
      return newAst

    Let x y -> do
      (newContext, newX) <- inferRecord x
      (_, newY) <- applyContext2 newContext (inferRecord y)
      return (annType (LetF newX newY) (attrType newY))

    Function {} -> do
        (Function param guard body) <- renameVarsInExpr ast
        (newContext, inType, newParam) <- addFuctionParamToContext param
        newGuard <- fromMaybeT Nothing $ do
          g <- MaybeT (return guard)
          ast <- lift $ localPut newContext (infer g)
          lift $ localPut newContext (unifyWithSubst (attrType ast) typeBool)
          return (Just ast)
        newAst <- applyContext2 newContext $ infer body
        inTypeApp <- applySubst inType
        let fTp = (TypeFunc inTypeApp (attrType newAst))
        return (annType (FunctionF newParam newGuard newAst) fTp)
        where
          fromMaybeT x = map (fromMaybe x) . runMaybeT

    Call func arg -> do
      funcAst <- infer func
      (outTp, argAst) <- applyContext (inferCall (attrType funcAst) arg)
      return ( annType (CallF funcAst argAst) outTp)

    Update arg labels -> do
      (funcTp, labelAsts) <- inferUpdateLabels labels
      (outTp, argAst) <- applyContext (inferCall funcTp arg)
      return (annType (UpdateF argAst labelAsts) outTp)

    SetStmt l r -> do
      (ctxL, inferLExpr) <- inferRecord l
      (ctxR, inferRExpr) <- inferRecord r
      unifyWithSubst (attrType inferRExpr) (attrType inferLExpr)
      return (annType (SetStmtF inferLExpr inferRExpr) NoType)

    If bExpr tExpr fExpr -> do
      (ctxB, inferBExpr) <- inferRecord bExpr
      unifyWithSubst (attrType inferBExpr) typeBool
      (ctxT, inferTExpr) <- applyContext2 ctxB (inferRecord tExpr)
      (ctxF, inferFExpr) <- applyContext2 ctxT (inferRecord fExpr)
      tType <- applySubst (attrType inferTExpr)
      unifyWithSubst tType (attrType inferFExpr)
      apptType <- applySubst tType
      return (annType (IfF inferBExpr inferTExpr inferFExpr) apptType)

    LabelAccess x -> do
      tv <- fresh
      tv2 <- fresh
      let tp = TypeFunc (TypeRow tv2 [TypeLabel x tv]) tv
      return (annType (LabelAccessF x) tp)

    Array list ->
      if onull list then do
        fv <- fresh
        let tp = typeArray fv
        return (annType (ArrayF []) tp)
      else do
      arrayAsts <- list & traverse infer
      let arrayTypes = arrayAsts <&> attrType
      let tp = typeArray $ simplifyUnion arrayTypes
      return (annType (ArrayF arrayAsts) tp)

    Cases body -> do
      newAst <- traverse infer body
      let funcType = joinFuncTypes (newAst <&> attrType)
      return (annType (CasesF newAst) funcType)

    x -> error $ unwords ["infer", show x]

inferUpdateLabels labels = do
    asts <- traverse inferLabel labels
    fv <- fresh
    let tp = TypeRow fv (asts <&> attrType)
    return (TypeFunc tp tp, asts)
  where
    inferLabel = \case
      RecordLabel label expr -> do
        ast <- infer expr
        let tp = TypeLabel label (attrType ast)
        return (annType (RecordLabelF label ast) tp)
      MD pos x -> local (position .~ Just pos) $ inferLabel x

inferCall t1 e2 = do
  ast2 <- infer e2
  tv <- fresh
  appt1 <- applySubst t1
  unifyWithSubst (TypeFunc (attrType ast2) tv) appt1
  rTp <- applySubst tv
  return (rTp, ast2)

inferLit = \case
  (LitInt _) -> TypeIdent "Int"
  (LitDouble _) -> TypeIdent "Double"
  (LitChar _) -> TypeIdent "Char"
  (LitString _) -> TypeIdent "String"
  (LitBool _) -> typeBool

-- addFuctionParamToContext context ast |traceArgs ["addFuctionParamToContext", show ast] = undefined
addFuctionParamToContext ast = case ast of
  MD md x -> local (position .~ Just md) $ addFuctionParamToContext x
  ParamIdent x -> do
    context <- ask
    tv <- fresh
    return (context & params %~ insertSet x & addIdentType x tv, tv, annType (ParamIdentF x) tv)
  ExprType (TypeLabel label expr) -> do
    context <- ask
    let newContext = context & params %~ insertSet label & addIdentType label expr
    return (newContext , expr, annType (ParamIdentF label) expr)
  Record list -> do
    context <- ask
    (newContext, tp, ast) <- C.foldM (\(context, tpList, astList) x -> do
                                      (newContext, tp, ast) <- localPut context $ addFuctionParamToContext x
                                      return (newContext, tpList ++ [tp], astList ++ [ast])) (context, [], []) list
    let paramTp = TypeRecord tp
    return (newContext, paramTp, annType (RecordF ast) paramTp)
  x -> error $ "addFunctionParamToContext " ++ show x

simplifyUnion list = list &ordNub & typeUnionIfSome

inferAllExpr :: InferContext -> Expr -> Either Error (InferContext, InferExpr)
inferAllExpr context ex = runInfer context (inferModule ex)


lookupIdentType :: Name -> Infer TypeExpr
--lookupIdentType x | traceArgs ["lookupIdentType", show x] = undefined
lookupIdentType x = fromMaybeM (typeError $ UnboundVariable x) $ do
  ctx <- ask
  tp <- MaybeT $ return $ getIdentType ctx x
  lift $ instantiate tp
  where
    fromMaybeM x y = runMaybeT y >>= maybe x return

addIdentType :: String -> TypeExpr -> InferContext -> InferContext
addIdentType name tp context = context & cmodule . idents %~ insertMap name (toScheme tp) --TODO repalce with Gen version

addTypeDeclRename name tp isFfi context = (\x -> context &addTypeDecl name x isFfi) <$> renameVarsInType tp

addIdentTypeRename name tp context = do
  x <- renameVarsInType tp
  addIdentTypeGen name x context

addIdentTypeGen :: String -> TypeExpr -> InferContext -> Infer InferContext
-- addIdentTypeGen name tp context | traceArgs ["addIdentTypeGen", show name, show tp, show $ context ^. idents]  = undefined
addIdentTypeGen name tp context = do
  x <- generalize context tp
  return $ context & cmodule . idents %~ insertMap name x

generalize :: InferContext -> TypeExpr -> Infer Scheme
generalize context t = do
  let as = setToList $ difference (ftv t) (ftv context)
  return $ Forall as t


findClassTypeList :: String -> [TypeExpr] -> [TypeExpr] -> Maybe TypeExpr
findClassTypeList var l1 l2 = zip l1 l2 &map (uncurry $ findClassType var) &catMaybes &headMay

findClassType :: String -> TypeExpr -> TypeExpr -> Maybe TypeExpr

-- findClassType var x y | traceArgs ["findClassType", show var, show x, show y] = undefined
findClassType var (TypeVar x) tp = if x == var then Just tp else Nothing
findClassType var (TypeRecord l1) (TypeRecord l2) = findClassTypeList var l1 l2
findClassType var (TypeConstrains _ x) y = findClassType var x y
findClassType var x (TypeConstrains _ y) = findClassType var x y
findClassType var (TypeFunc x y) (TypeFunc x2 y2) = findClassTypeList var [x,y] [x2,y2]
findClassType var (TypeApply x y) ast@(TypeApply {}) =
  let TypeApply x2 y2 = curryTypeApply (olength y) ast in findClassTypeList var (x : y) (x2 : y2)
-- findClassType var (TypeApply x y) z = findClassType var x z
findClassType var x y = error $ unwords ["findClassType", var, show x, show y]

getInstanceType :: InferContext -> String -> TypeExpr -> Infer TypeExpr
getInstanceType context name tp = do
  (var, classType) <- lookupClass name context & maybe (typeError $ UnboundClass name) return
  findClassType var classType tp & maybe (typeError $ TextError ("cannot find class type for " ++ name)) return

importModule :: ModuleName -> Maybe String -> InferContext -> Infer InferContext
importModule name asName context = fromMaybe (typeError $ UnboundModule name) $ do
  mdl <- context & lookupModule name
  return $ asName & maybe (openIdent name context) (\x -> return $ importModuleToContext context x mdl)

typeToScheme :: TypeExpr -> Scheme
typeToScheme t = Forall (setToList $ ftv t) t

schemeToModule ident (Forall vars t) = go (newModule ident) t -- TODO vars???
  where
    go mdl (TypeLabel name tp) = mdl & idents %~ insertMap name (typeToScheme tp)
    go mdl (TypeRecord list) = list & foldl' go mdl
    go mdl _ = mdl

identToModule ident ctx = (ctx & lookupModule ident) <|> (schemeToModule ident <$> getIdentType ctx (moduleNameToIdent ident))

openIdent :: ModuleName -> InferContext -> Infer InferContext
openIdent ident ctx = fromMaybe (typeError $ UnboundVariable (ident & moduleNameToIdent)) $ do
  mdl <- identToModule ident ctx
  return $ return $ addOpenModule ident mdl ctx

includeIdent :: ModuleName -> InferContext -> Infer InferContext
includeIdent ident ctx = fromMaybe (typeError $ UnboundVariable (moduleNameToIdent ident)) $ do
  mdl <- identToModule ident ctx
  return $ return $ includeModule ident mdl ctx


getTypeDecl :: String -> InferContext -> Infer TypeExpr
getTypeDecl name context = fromMaybe (typeError $ UnboundType name) $ do
  (tp, isFfi) <- context & lookupType name
  if isFfi then return $ return (TypeIdent name)
  else return $ return tp

-- addTypeDeclCheck context name tp | traceArgs ["addTypeDeclCheck", show name, show tp] = undefined
addTypeDeclCheck context name tp = do
  hasUnionType <- isUnionWithUnion tp
  if hasUnionType then (typeError $ TypeUnionWithUnion name) else context & addTypeDeclRename name tp False
  where
    isUnionWithUnion (TypePoly _ (TypeUnion list)) = or <$> (list & traverse typeIdentIsUnion)
    isUnionWithUnion (TypeUnion list) = or <$> (list & traverse typeIdentIsUnion)
    isUnionWithUnion _ = return False
    typeIdentIsUnion t = fromMaybe (return False) $ do
      x <- getTypeIdent t
      return $ isTypeUnion <$> getTypeDecl x context

inferModule :: Expr -> Infer (InferContext, InferExpr)
-- inferModule context x | traceArgs ["inferModule"] = undefined
inferModule ast = do
  (newContext, recordAsts) <- inferRecord ast
  newAst <- applySubst recordAsts
  return (newContext & cmodule %~ normalizeModule, newAst)

inferRecord :: Expr -> Infer (InferContext, InferExpr)
inferRecord (Record x) = do
  ctx <- ask
  (newContext, recordAsts) <- x & C.foldM inferRecordPart (ctx, [])
  let tp = TypeRecord (recordAsts <&> attrType)
  return (newContext, annType (RecordF recordAsts) tp)
inferRecord x = do
  ctx <- ask
  (newContext, [ast]) <- inferRecordPart (ctx, []) x
  return (newContext, ast)

updateContext expr = do
  context <- ask
  case expr of
    (MD pos y) -> local (position .~ Just pos) $ updateContext y
    (Stmt (StmtImport name asName)) -> importModule name asName context
    (Stmt (StmtOpen name)) -> openIdent [name] context
    (Stmt (StmtInclude name)) -> includeIdent name context
    (Ffi name expr) -> context & addIdentTypeRename name expr
    (FfiType name expr) -> addTypeDeclRename name expr True context
    (TypeDecl name tp) -> addTypeDeclCheck context name tp -- TODO add type decl schema
    (ClassFn name tp) -> do
      let (TypePoly [TypeVar var] t) = tp
      identType <- renameVarsInType (TypeConstrains [(TypeVar var, name)] t)
      context & cmodule . classes %~ insertMap name (var, t) & addIdentTypeGen name identType
    (InstanceFn tp name _) ->
      return $ context
        & cmodule . instances %~ alterMap (\x -> Just ((maybeToList x & concat) ++ [name])) tp 
        & cmodule . instancesType %~ insertMap (instanceTypeName tp, name) tp
    _ -> return context

-- inferRecordPart (context, s, acc, accExpr) expr | traceArgs ["inferRecordPart", show expr] = undefined

inferRecordPart (c, accExpr) expr = do
  ast <- localPut c $ infer expr
  newContext <- applyContext2 c (updateContext expr)
  newContext2 <-
        case attrType ast of
          (TypeLabel name eType) -> newContext & addIdentTypeGen name (moveConstrainsOnTop eType)
          _ -> return newContext
  return (newContext2,  accExpr ++ [ast])

toScheme = Forall []

joinTypes :: [TypeExpr] -> TypeExpr -> [TypeExpr]
joinTypes list x = list ++ [x]

putInUnion (TypeUnion list) tp  = TypeUnion (list ++ [tp])
unionRecordToRecordUnion tp@(TypeUnion x) = C.foldM go (TypeRecord []) x & fromMaybe tp
  where
    go (TypeRecord []) (TypeRecord list) = Just $ TypeRecord (list &map (\x -> TypeUnion [x]))
    go (TypeRecord list) (TypeRecord uList) | length list == length uList = Just $ TypeRecord $ zipWith (putInUnion) list uList
    go x y = Nothing
unionRecordToRecordUnion tp = tp

joinFuncTypes [t] = t
joinFuncTypes list =
  TypeFunc (unionRecordToRecordUnion $ typeUnionIfSome $ ordNub inTypes) (unionRecordToRecordUnion $ typeUnionIfSome $ ordNub outTypes) --TODO add union unification
  where
    go (inTypes, outTypes) (TypeFunc inType outType) =
                  ((joinTypes inTypes inType), (joinTypes outTypes outType))
    go x y = error $ unwords ["joinFuncType", show y]
    (inTypes, outTypes) = list &foldl' go ([], [])

normalizeModule context = context & idents %~ (map normalize)
normalizeType t = renameTypeVars m t
  where
    m = zip (ordNub $ setToList $ ftv t) letters & mapFromList
normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ordList) (renameTypeVars m body)
  where
    ordList = zip (ordNub $ setToList $ ftv body) letters
    m = mapFromList ordList

match t1 t2 = applyContext $ runUnify t1 t2
unifyWithSubst t1 t2 = do
  x <- applyContext $ runUnify t1 t2
  addSubst x

applyContext x = do
  ctx <- ask
  newCtx <- applySubst ctx
  localPut newCtx x

applyContext2 ctx x = do
  newCtx <- applySubst ctx
  localPut newCtx x
