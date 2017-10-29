module Oczor.Infer.Infer (module Oczor.Infer.Infer, module X) where

import Oczor.Syntax.Syntax
import Oczor.Utl
import Oczor.Infer.Unify
import Control.Monad.RWS
import ClassyPrelude as C
import qualified Data.Map as M
import Oczor.Infer.InferContext as X
import Oczor.Infer.State
import Oczor.Infer.InferAst as X
import Oczor.Infer.Substitutable
import Control.Monad.Trans.Maybe

infer :: Expr -> Infer InferExpr
infer ast = {-trac ("inferResult " ++ show ast) <$>-}
  liftA2 changeContext ask r

  where

  r = case ast of
    -- ast | traceArgs (["infer", show ast]) -> undefined
    MD pos x -> local (position .~ Just pos) $ infer x

    WithType {} -> do
      WithTypeF ast @ (Ann x _) tp <- traverse infer $ project ast
      renamedTP <- renameVarsInType tp
      unifyWithSubst (attrType ast) renamedTP
      annType x <$> applySubst renamedTP -- TODO think WithType

    Record x -> snd <$> inferRecord ast

    Let x y -> do
      (newContext, newX) <- inferRecord x
      (_, newY) <- applyContext2 newContext (inferRecord y)
      return (annType (LetF newX newY) (attrType newY))

    Function param guard body -> do
        (newContext, inType, newParam) <- addFuctionParamToContext param
        newGuard <- for guard $ \g -> do
             a <- localPut newContext (infer g)
             localPut newContext (unifyWithSubst (attrType a) typeBool)
             return a
        newAst <- applyContext2 newContext $ infer body
        inTypeApp <- applySubst inType
        let fTp = TypeFunc inTypeApp (attrType newAst)
        return (annType (FunctionF newParam newGuard newAst) fTp)

    SetStmt l r -> do
      (ctxL, inferLExpr) <- inferRecord l
      (ctxR, inferRExpr) <- inferRecord r
      (unifyWithSubst `on` attrType) inferRExpr inferLExpr
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

    RecordLabel name _ -> do
      fv <- fresh
      term @ (RecordLabelF _ newAst) <- local (addIdentType name fv) $ traverse infer $ project ast
      let tp = attrType newAst
      appFv <- applySubst fv
      unifyWithSubst appFv tp
      annType term <$> applySubst (TypeLabel name tp)

    _ -> do
      term <- traverse infer $ project ast
      annType term <$> inferPhi term

-- "phi" is a reference to future cata
inferPhi = \case
  StmtF {} -> return NoType
  FfiF {} -> return NoType
  FfiTypeF {} -> return NoType
  ClassFnF {} -> return NoType
  TypeDeclF {} -> return NoType

  InstanceFnF tp name ast -> do
    let t = attrType ast
    context <- ask
    instanceType <- getInstanceType context name t
    match instanceType tp
    -- newType <- applySubst t -- TODO
    return t

  LitF x -> return $ inferLit x
  UniqObjectF {} -> fresh
  IdentF x -> lookupIdentType x

  LabelAccessF x -> do
    tv <- fresh
    tv2 <- fresh
    return $ TypeFunc (TypeRow tv2 [TypeLabel x tv]) tv

  ArrayF [] -> typeArray <$> fresh

  ArrayF arrayAsts -> return $ typeArray $ simplifyUnion $ map attrType arrayAsts

  CasesF newAst -> return $ joinFuncTypes (newAst <&> attrType)

  CallF funcAst argAstOld -> fst <$> applyContext (inferCall (attrType funcAst) argAstOld)

  UpdateF argAstOld labelsAstsOld -> do
    (funcTp, labelAsts) <- inferUpdateLabels labelsAstsOld
    fst <$> applyContext (inferCall funcTp argAstOld)

  x -> error $ unwords ["infer", show x]

inferUpdateLabels asts = do
    fv <- fresh
    let tp = TypeRow fv (asts <&> attrType)
    return (TypeFunc tp tp, asts)

inferCall t1 ast2 = do
  tv <- fresh
  appt1 <- applySubst t1
  unifyWithSubst (TypeFunc (attrType ast2) tv) appt1
  rTp <- applySubst tv
  return (rTp, ast2)

inferLit = \case
  LitInt _ -> TypeIdent "Int"
  LitDouble _ -> TypeIdent "Double"
  LitChar _ -> TypeIdent "Char"
  LitString _ -> TypeIdent "String"
  LitBool _ -> typeBool

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
  tp <- MaybeT . return $ getIdentType ctx x
  lift $ instantiate tp
  where
    fromMaybeM x y = runMaybeT y >>= maybe x return

addIdentType :: String -> TypeExpr -> InferContext -> InferContext
addIdentType name tp = cmodule . idents %~ insertMap name (toScheme tp) --TODO repalce with Gen version

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
generalize context t = return $ Forall (setToList $ difference (ftv t) (ftv context)) t

findClassTypeList :: String -> [TypeExpr] -> [TypeExpr] -> Maybe TypeExpr
findClassTypeList var l1 l2 =  zipWith (findClassType var) l1 l2 & catMaybes & headMay

findClassType :: String -> TypeExpr -> TypeExpr -> Maybe TypeExpr

-- findClassType var x y | traceArgs ["findClassType", show var, show x, show y] = undefined
findClassType var (TypeVar x) tp = if x == var then Just tp else Nothing
findClassType var (TypeRecord l1) (TypeRecord l2) = findClassTypeList var l1 l2
findClassType var (TypeConstraints _ x) y = findClassType var x y
findClassType var x (TypeConstraints _ y) = findClassType var x y
findClassType var (TypeFunc x y) (TypeFunc x2 y2) = findClassTypeList var [x,y] [x2,y2]
findClassType var (TypeApply x y) ast@TypeApply {} =
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
  return . return $ addOpenModule ident mdl ctx

includeIdent :: ModuleName -> InferContext -> Infer InferContext
includeIdent ident ctx = fromMaybe (typeError $ UnboundVariable (moduleNameToIdent ident)) $ do
  mdl <- identToModule ident ctx
  return . return $ includeModule ident mdl ctx


getTypeDecl :: String -> InferContext -> Infer TypeExpr
getTypeDecl name context = fromMaybe (typeError $ UnboundType name) $ do
  (tp, isFfi) <- context & lookupType name
  return . return $ if isFfi then TypeIdent name else tp

-- addTypeDeclCheck context name tp | traceArgs ["addTypeDeclCheck", show name, show tp] = undefined
addTypeDeclCheck context name tp = do
  hasUnionType <- isUnionWithUnion tp
  if hasUnionType then typeError $ TypeUnionWithUnion name else context & addTypeDeclRename name tp False
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
    MD pos y -> local (position .~ Just pos) $ updateContext y
    Stmt (StmtImport name asName) -> importModule name asName context
    Stmt (StmtOpen name) -> openIdent [name] context
    Stmt (StmtInclude name) -> includeIdent name context
    Ffi name expr -> context & addIdentTypeRename name expr
    FfiType name expr -> addTypeDeclRename name expr True context
    TypeDecl name tp -> addTypeDeclCheck context name tp -- TODO add type decl schema
    ClassFn name tp -> do
      let (TypePoly [TypeVar var] t) = tp
      identType <- renameVarsInType (TypeConstraints [(TypeVar var, name)] t)
      context & cmodule . classes %~ insertMap name (var, t) & addIdentTypeGen name identType
    InstanceFn tp name _ ->
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
          (TypeLabel name eType) -> newContext & addIdentTypeGen name (moveConstraintsOnTop eType)
          _ -> return newContext
  return (newContext2,  accExpr ++ [ast])

toScheme = Forall []

joinTypes :: [TypeExpr] -> TypeExpr -> [TypeExpr]
joinTypes list x = list ++ [x]

putInUnion (TypeUnion list) tp  = TypeUnion (list ++ [tp])
unionRecordToRecordUnion tp@(TypeUnion x) = C.foldM go (TypeRecord []) x & fromMaybe tp
  where
    go (TypeRecord []) (TypeRecord list) = Just $ TypeRecord (list & fmap (\x -> TypeUnion [x]))
    go (TypeRecord list) (TypeRecord uList) | length list == length uList = Just . TypeRecord $ zipWith putInUnion list uList
    go x y = Nothing
unionRecordToRecordUnion tp = tp

joinFuncTypes [t] = t
joinFuncTypes list =
  TypeFunc (unionRecordToRecordUnion . typeUnionIfSome $ ordNub inTypes) (unionRecordToRecordUnion . typeUnionIfSome $ ordNub outTypes) --TODO add union unification
  where
    go (inTypes, outTypes) (TypeFunc inType outType) =
                  (joinTypes inTypes inType, joinTypes outTypes outType)
    go x y = error $ unwords ["joinFuncType", show y]
    (inTypes, outTypes) = list &foldl' go ([], [])

normalizeModule context = context & idents %~ fmap normalize
normalizeType t = renameTypeVars m t
  where
    m = mapFromList $ zip (otoList $ ftv t) letters
normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (M.elems mm) (renameTypeVars mm body)
  where
    mm = mapFromList $ zip (otoList $ ftv body) letters

match t1 t2 = applyContext $ runUnify t1 t2

unifyWithSubst t1 t2 = match t1 t2 >>= addSubst

applyContext x = ask >>= (`applyContext2` x)

applyContext2 ctx x = applySubst ctx >>= (`localPut` x)
