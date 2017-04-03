module Oczor.Converter.Converter where
import Oczor.Utl
import Control.Monad.State
import Control.Monad.Reader
import ClassyPrelude hiding (first)
import Oczor.Infer.Substitutable
import Oczor.Syntax.Syntax as S
import qualified Oczor.Converter.CodeGenAst as A
import Oczor.Infer.InferContext as T
import Oczor.Infer.Infer
import qualified Data.List as L

-- typeFuncArity x | traceArgs ["typeFuncArity", show x] = undefined
typeFuncArity = \case
  (TypeFunc x _) -> typeArity x
  (TypeConstraints _ y) -> typeFuncArity y
  TypePoly _ (TypeFunc x _) -> typeArity x
  _ -> 0

rootModule = A.Ident "oc"

type Converter = Reader InferContext

identWithNamespace isModule = \case
  [h] | not isModule -> A.Ident h
  x -> foldl' A.Field rootModule x


initObjectIfNull :: A.Ast -> A.Ast
initObjectIfNull x = A.If (A.Equal x (A.Lit A.LitNull)) [A.Set x A.emptyObject] []

initModuleIfNull moduleName = L.init moduleName & L.inits & L.tail <&> identWithNamespace True <&> initObjectIfNull

typeNormalizeEq = (==) `on` normalizeType

curryApply :: [A.Ast] -> Int -> A.Ast -> A.Ast
curryApply args arity func =
  if hasParams then A.Function params [A.Return (A.Call func (args ++ (params <&> A.Ident)))]
  else A.Call func args
  where
    hasParams = arity > 0
    params = if hasParams then [1..arity] & map (show >>> ("p" ++)) else []

instancesObject = A.Field $ A.Field rootModule "instances"

-- applyParam context x | traceArgs ["applyParam", show x] = undefined
applyParam context (TypeApply (TypeIdent ident) y) = makeType t y
  where
    t = context & T.lookupType ident & map fst & fromMaybe (error "applyParam")

-- findInstancesList :: String -> [TypeExpr] -> [TypeExpr] -> Maybe TypeExpr
-- findInstances context l1 l2 | traceArgs ["findInstaces", show l1, show l2] = undefined
findInstances context l1 l2 = go mempty l1 l2 & ordNub & map (\(_, x, y) -> (x, y))
  where
  goList vars l1 l2 = zip l1 l2 >>= uncurry (go vars)
  go :: Map String [String] -> TypeExpr -> TypeExpr -> [(String, String, TypeExpr)]
  -- go var x y | traceArgs ["findInstances go", show var, show x, show y] = undefined
  go var x (TypeConstraints _ y) = go var x y
  go var (TypeVar x) tp = (var &lookup x) & maybe [] (\clist -> clist &map (\cls -> (x, cls, tp)))
  go var (TypeRecord l1) (TypeRecord l2) = goList var l1 l2
  go var (TypeRecord [x]) y = go var x y
  go var (TypeRow x l1) (TypeRow y l2) = goList var (l1 ++ [x]) (l2 ++ [y])
  go var (TypeConstraints list x) y = go (unionWith (++) var (constraintSetToMap list)) x y
  go var (TypeFunc x y) (TypeFunc x2 y2) = goList var [x,y] [x2,y2]
  go var (TypeApply x y) (TypeApply x2 y2) = goList var (x:y) (x2:y2)
  go var (TypeLabel x t1) (TypeLabel y t2) | x == y = go var t1 t2
  go var (TypeIdent x) (TypeIdent y) | x == y = []
  go var z x@(TypeApply t _) | not $ T.isFfiType t context = go var z (applyParam context x)
  go var x y = error $ unwords ["findInstances", show var, show x, show y]


-- instanceIdent cls tp exprTp | traceArgs ["instanceIdent", show cls, show tp, show exprTp] = undefined
instanceIdent :: _ -> _ -> _ -> Converter A.Ast
instanceIdent cls tp exprTp = do
  context <- ask
  let (_, classType) = context & lookupClass cls & unsafeHead
  let instanceTypeIdent = instanceTypeName tp
  let instanceType = context & T.lookupInstanceType instanceTypeIdent cls & fromMaybe (error "instanceIdent") -- & trac "instaceType"
  let typeConstraints = getTypeConstraints instanceType -- & trac "typeconstraints"
  let expr = A.Field (instancesObject cls) instanceTypeIdent
  if onull typeConstraints then return expr
  else addInstancesArgs (typeFuncArity classType) expr instanceType exprTp

-- addInstancesArgs arity expr contextTp exprTp | traceArgs ["addInstacesArgs", show arity, show expr, show contextTp, show exprTp] = undefined
addInstancesArgs arity expr contextTp exprTp =
  if contextTp == exprTp then return expr
  else do
    args <- instancesToArgs contextTp exprTp -- <&> trac "instances"
    if null args then return expr else return $ curryApply args arity expr

addInstancesParams :: InferExpr -> Converter A.Ast
-- addInstancesParams context expr | traceArgs ["addInstacesParams", show expr] = undefined
addInstancesParams expr = do
  let clist =  collectAllConstraints expr -- & trac "constraints"
  let clistParam = clist & map (\(TypeVar p, cl) -> paramInstancesName p cl)
  targetExpr <- convert expr
  return $ if onull clistParam then targetExpr else
    case targetExpr of
      A.Function x y -> A.Function (clistParam ++ x) y
      _ -> A.Function clistParam [A.Return targetExpr]

-- instancesToArgs context contextTp exprTp | traceArgs ["instancesToArgs", show contextTp, show exprTp] = undefined
instancesToArgs contextTp exprTp =
  -- if contextTp == exprTp then return []
  -- else 
  let classes = getTypeConstraints contextTp in
  if onull classes then return []
  else do
    context <- ask
    let instances = findInstances context contextTp exprTp
    instances &traverse go
    where
      go (cls, TypeVar x) = return $ A.Ident $ paramInstancesName x cls
      go (cls, tp) = instanceIdent cls tp tp

isLazy = getTypeIdent >>> maybe False (== "Lazy")

-- convertExprWithTypeChange tp expr | traceArgs ["convertExprWithTypeChange", show tp, show expr] = undefined
-- convertExprWithTypeChange ctx tp exprTp expr =
--   if T.isFfiType exprTp ctx then (A.Field expr "value")
--   else A.Call (A.Ident "convertExprWithTypeChange") [expr, litString $ typeString exprTp]
cloneObjectCall x = A.Call (A.Field rootModule "cloneObject") [x]

convertExprWithTypeChange tp exprTp expr
  | isLazy tp = A.Call expr []
  | isLazy exprTp = A.Function [] [A.Return expr]
  | otherwise = expr

convertExprWithNewTypeMaybe tp exprTp expr = maybe expr (\x -> convertExprWithTypeChange tp x expr) exprTp

-- isUnionSubtype context identTp t | traceArgs ["isUnionSubtype", show identTp, show t] = undefined
isUnionSubtype context identTp t | (getTypeIdent t & isJust) && not (T.isFfiType t context) =
  case context & T.lookupType (getTypeIdent t & unsafeHead) & unsafeHead & fst of
    (TypeUnion list) -> list & any (typeNormalizeEq identTp)
    (TypePoly _ (TypeUnion list)) -> list & any (typeNormalizeEq identTp)
    _ -> False
isUnionSubtype _ _ _ = False


-- isTypeUnionOrMaker x | traceArgs ["isTypeUnionOrMaker", show x] = undefined
isTypeUnionOrMaker (TypeUnion list) = True
isTypeUnionOrMaker (TypePoly _ (TypeUnion list)) = True
isTypeUnionOrMaker _ = False

checkExprTypeChange :: _ -> _ -> _ -> Converter _
checkExprTypeChange tp exprTp expr = do
  context <- ask
  let typeIdentIsUnion x = getTypeIdent x & maybe False (\x -> context & T.lookupType x & unsafeHead & fst & isTypeUnionOrMaker)
  return $
    if exprTp == NoType || typeNormalizeEq tp exprTp || typeIdentIsUnion exprTp || hasTypeVar tp then expr
    else convertExprWithTypeChange tp exprTp expr

paramInstancesName ident cls = sysPrefix ++ ident ++ cls

-- getIdentInstancesArgs ident exprTp |traceArgs ["getIdentInstancesArgs", show ident, show exprTp] = undefined
getIdentInstancesArgs ident exprTp = do
  ctx <- ask
  identTp <- identType ident -- <&> trac (unwords ["ident tp", ident, show exprTp] ) 
  -- if identTp == exprTp then return []
  -- else instancesToArgs identTp exprTp
  if ctx ^. T.params & member ident then return []
  else instancesToArgs identTp exprTp
  -- instancesToArgs identTp exprTp


-- getTypeInstancesArgs contextTp exprTp = do
--   let classes = getTypeConstraints contextTp
--   if onull classes then return []
--   else instancesToArgs contextTp exprTp

identAddInstancesArgs ident exprTp = do
  ctx <- ask
  -- instancesArgs <- getIdentInstancesArgs ident exprTp
  identTp <- identType ident -- <&> trac (unwords ["ident tp", ident, show exprTp] )
  expr <- newIdent ident
  expr2 <- checkExprTypeChange identTp exprTp expr
  -- if exprTp == identTp then return expr2
  -- else addInstancesArgs (typeFuncArity identTp) expr2 identTp exprTp
  addInstancesArgs (typeFuncArity identTp) expr2 identTp exprTp

identType :: String -> Converter TypeExpr
identType x = do
  context <- ask
  return $ T.getIdentType context x & map (\(Forall _ x) -> x) & fromMaybe NoType

getTypeConstraints :: TypeExpr -> ConstraintSet
getTypeConstraints = para $ \case
    TypeConstraintsF list (TypeLabel x y, _) -> []
    TypeConstraintsF list x -> list <&> first fst
    x -> ffold $ getResults x

getResults :: TypeExprF (a, b) -> TypeExprF b
getResults = map snd

removeResults :: TypeExprF (a, b) -> TypeExprF a
removeResults = map fst

newIdent :: String -> Converter A.Ast
newIdent ident = identWithNamespace False . (<> [ident]) . fromMaybe [] . lookup ident <$> view (openModules . identsNs)

collectAllConstraints :: InferExpr -> [(TypeExpr, String)]
-- collectAllConstraints x | traceArgs ["collectAllConstraints", pshow x] = undefined
collectAllConstraints (UnAnn RecordF {}) = []
collectAllConstraints (UnAnn RecordLabelF {}) = []
-- collectAllConstraints (UnAnn IdentF {}) = []
collectAllConstraints (UnAnn (CallF (UnAnn LabelAccessF {}) _)) = [] -- TODO why?
-- collectAllConstraints (UnAnn (LabelAccessCallF {})) = []
collectAllConstraints x = y x & ordNub
  where
  y = cata $ \case
    -- x | traceArgs ["collect alg", show x] -> undefined
    (AnnF y (x,ctx)) -> collectConstrainFromTypeExpr x ++ ffold y


convertModule ctx expr moduleName ffiCode =
  A.StmtList $ initModuleIfNull moduleName ++
    [A.Set (identWithNamespace True moduleName) newModuleAst]
  where
    moduleAst = case convert2 ctx expr of {A.None -> A.emptyObject; x -> x}
    newModuleAst = case ffiCode of
      Nothing -> moduleAst
      Just code ->
        case moduleAst of
          A.Scope x y -> A.Scope (A.Code code : x) y
          x -> A.Scope [A.Code code] x

-- convert2 ctx expr | traceArgs ["convert context", pshow $ ctx ^. openModules] = undefined
convert2 ctx expr = runReader (convert expr) ctx

convert :: InferExpr -> Converter A.Ast
convert annAst@(Ann ast (tp, ctx)) = localPut ctx $ go ast where
  -- go x | traceArgs $ ["convert", show x]  = undefined
  go = \case
     IdentF ident -> identAddInstancesArgs ident tp
     UniqObjectF x -> return $ A.UniqObject x
     FunctionF {} -> convertFunction ast (Just outtp) where (TypeFunc _ outtp) = tp
     CasesF expr -> cases expr Nothing -- TODO add out type
     LitF value -> checkExprTypeChange (inferLit value) tp (convertLit value)
     RecordLabelF name expr -> (A.Object . (:[])) <$> convertRecordLabel ast
     CallF {} -> convertCall annAst
     UpdateF {} -> convertUpdate ast
     SetStmtF l r -> do
       lAst <- convert l
       rAst <- convert r
       return $ A.Set lAst rAst
     IfF b t f -> do
       bAst <- convert b
       tAst <- convert t
       fAst <- convert f
       return $ A.ConditionOperator bAst tAst fAst
     RecordF list -> newScope <$> convertAstList list
     ArrayF x -> A.Array <$> (x & traverse convert)
     ClassFnF name _ -> return $ convertClass ast
     LetF e1 e2 -> do
       e1Ast <- evalStateT (convertRecordToVars e1) 1
       e2Ast <- convert e2
       return $ newScope (partsToAsts e1Ast, e2Ast)
     InstanceFnF tpName name body ->
      A.setField (instancesObject name) (instanceTypeName tpName) <$> addInstancesParams body
     ExprTypeF {} -> return A.None
     TypeDeclF {} -> return A.None
     FfiF {}  -> return $ A.Object [convertFfiLabel ast]
     FfiTypeF {} -> return A.None
     StmtF {} -> return A.None
     x -> error $ unwords ["convert", show x]

convertUpdate (UpdateF expr labels) = do
  temp <- convert expr
  let obj = A.Ident "_obj"
  let objAndClone = [A.Var "_obj" temp, A.Var "_clone" (cloneObjectCall obj)]
  temp2 <- labels & traverse go
  return $ newScope (objAndClone ++ temp2, A.Ident "_clone")
  where
    go (UnAnn (RecordLabelF label value)) = A.setField (A.Ident "_clone") label <$> convert value

-- convertRecordLabel context x | traceArgs ["convertRecordLabel", show x] = undefined
convertRecordLabel (RecordLabelF name expr) = do
  context <- ask
  let exprType = (\(S.Forall _ x) -> x) <$> T.getIdentType context name
  (name,) <$> addInstancesParams expr

convertFfiLabel (FfiF name expr) = (name, A.Ident name)

convertFunction (FunctionF params guard body) newOutType =
  A.Function (convertFuncParams params) <$> functionBody body newOutType

-- convertCallLabel CallF (UnAnn (LabelAccessF label)) expr -> convert expr <&> flip A.Field label

-- convertCall (Ann (CallF (UnAnn (LabelAccessF label)) (UnAnn (IdentF ident))) (exprTp,ctx)) | traceArgs ["convertCall", label, show exprTp] = undefined
convertCall (Ann (CallF (UnAnn (LabelAccessF label)) (UnAnn (IdentF ident))) (exprTp,ctx)) = do
  expr <- A.Field <$> newIdent ident <*> return label
  let arity = typeFuncArity exprTp
  contextTp <- identType ident <&> getLabelType label
  maybe (return expr ) (\contextTp -> addInstancesArgs arity expr contextTp exprTp) contextTp
convertCall (UnAnn (CallF expr args)) = do
  callArgs <- convertFuncArgs args
  case expr of
    (Ann (IdentF ident) (tp, ctx)) -> do
      instanceArgs <- localPut ctx $ getIdentInstancesArgs ident tp
      exprCode <- newIdent ident
      let isClass = ctx & lookupClass ident & isJust
      return $ if isClass && length instanceArgs == 1 then A.Call (unsafeHead instanceArgs) callArgs else A.Call exprCode (instanceArgs ++ callArgs)
    _ -> do
      exprCode <- convert expr
      return $ A.Call exprCode callArgs



convertClass (ClassFnF name body) =
  A.StmtList [A.Set (instancesObject name) (A.Object []),
              A.Label name codeBody]
  where
    clsArity = typeFuncArity body
    hasParams = clsArity > 0
    params = if hasParams then [1..clsArity] & map (show >>> ("p" ++)) else []
    codeBody =
      if hasParams then A.Function ("x" : params) [A.Return (A.Call (A.Ident "x") (params <&> A.Ident))]
      else A.Function ["x"] [A.Return (A.Ident "x")]

-- newScope (x,y) = if onull x then y else (A.Call (A.Parens (A.Function [] $ x ++ [A.Return y])) [])
newScope (x,y) = if onull x then y else A.Scope x y

functionBody expr@(Ann (RecordF l) (tp,ctx)) newOutType = localPut ctx $ do
  (body, ret) <- convertAstList l
  let r = convertExprWithNewTypeMaybe tp newOutType ret
  return $ body ++ [A.Return ret]
functionBody expr@(Ann _ (tp,ctx)) newOutType = localPut ctx $ do
  y <- convert expr
  case y of
    (A.Scope body ret) -> return $ body ++ [A.Return $ convertExprWithNewTypeMaybe tp newOutType ret]
    _ -> return [A.Return $ convertExprWithNewTypeMaybe tp newOutType y]

convertFuncParams :: InferExpr -> [String]
convertFuncParams (UnAnn (RecordF list)) = list >>= convertFuncParams
convertFuncParams (UnAnn (ParamIdentF x)) = [x]

convertFuncArgs = \case
  UnAnn (RecordF list) ->  list &traverse convert
  x -> (:[]) <$> convert x

convertAstList list = do
  parts <- evalStateT (convertRecordToVarsList list) 1 -- & trac "parts"
  let hasStmts = parts & any (fst >>> isNothing)
  let labels = parts & map fst & catMaybes & filter (snd >>> not) & map fst
  let asts = parts & partsToAsts
  let funcLabels = parts & filter (snd >>> A.isFunction) & map fst & catMaybes & map fst
  let labelsWithIdents = asts >>= A.containsIdents labels
  let labelsInScope = if notNull labelsWithIdents || hasStmts then labelsWithIdents ++ funcLabels else []
  let returnObject = parts & filter (fst >>> isJust) & map (\(Just (x, _), y) -> if elem x labelsInScope then (x, A.Ident x) else (x, y)) & A.Object
  return (parts & filter (fst >>> maybe True (fst >>> (`elem` labelsInScope))) & map partToAst, returnObject)

type RecordParts = (Maybe (String, T.IsFfi), A.Ast)

partToAst (x,y) = maybe y (\(x,isFfi) -> if isFfi then A.None else A.Var x y) x
partsToAsts parts = parts & map partToAst

astToPart = \case
  A.Label x y -> (Just (x,False), y)
  x -> (Nothing, x)

convertRecordToVarsList :: [InferExpr] -> StateT Int Converter [RecordParts]
convertRecordToVarsList list =
  list & traverse convertRecordToVars & map (concat >>> filter (snd >>> (/= A.None)))
convertRecordToVars :: InferExpr -> StateT Int Converter [RecordParts]
convertRecordToVars ast@(Ann ex (tp, ctx)) = case ex of
  RecordF list -> list & traverse convertRecordToVars & map concat
  RecordLabelF {} -> do
    (x,y) <- lift $ convertRecordLabel ex
    return [(Just (x,False), y)]
  FfiF {} -> do
    let (x,y) = convertFfiLabel ex
    return [(Just (x,True), y)]
  _ | isExpr ex -> tupleLabel ast
  _ -> do
    temp <- lift $ convert ast
    return $ temp & A.astToList & map astToPart
  where
    tupleLabel ast = do
      index <- get
      put (index + 1)
      x <- lift $ convert ast
      return [(Just ("item" ++ show index, False), x)]
    isExpr = \case
      IdentF {} -> True
      LitF {} -> True
      CasesF {} -> True
      LetF {} -> True
      CallF {} -> True
      UpdateF {} -> True
      FunctionF {} -> True
      ArrayF {} -> True
      IfF {} -> True
      _ -> False

newBoolAnds list = if olength list > 1 then A.BoolAnds list else list & unsafeHead

funcParamCond = \case
  Ann (ParamIdentF x) (tp, cxt) | notNull typeLabels ->
   map (A.HasField (A.Ident x)) typeLabels
    where typeLabels = getTypeLabels tp
  UnAnn (RecordF list) -> list >>= funcParamCond
  _ -> []

casesFuncs newOutType (UnAnn f@(FunctionF params guard _)) = do
  temp1 <- maybe (return []) (\x -> (:[]) <$> convert x) guard
  let conds = funcParamCond params ++ temp1
  func@(A.Function aparams body) <- convertFunction f newOutType
  if onull conds then return (func, Nothing)
  else return (A.Function aparams body, Just $ A.Function aparams [A.Return $ newBoolAnds conds])



-- casesDiffParamTypes :: Functor f => f (Fix (AnnF ExprF b)) -> f b
-- casesDiffParamTypes :: [InferExpr] -> [TypeExpr]
-- casesDiffParamTypes list = list & map (\(UnAnn (FunctionF (Ann _ tp) _ _)) -> tp)

cases list newOutType = do
  funcsWithCond <- list & traverse (casesFuncs newOutType)
  let hasNoCondFunc = funcsWithCond & any (snd >>> isNothing)
  let arity = list & map stripAnns & casesArity
  let count = if olength arity == 1 then arity & unsafeHead else error "cases arity length <> 1"
  let params = [1..count] & map (\i -> sysPrefix ++ "a" ++ show i)
  let paramIdents = params & map A.Ident
  let init = if hasNoCondFunc then [] else [A.Throw "cases error"]
  let funcCondCode (x, cond) acc =
        let r = A.Call x paramIdents in
          maybe [A.Return r] (\cond -> [A.If (A.Call cond paramIdents) [A.Return r] acc]) cond
  let funcIfs = foldr funcCondCode init funcsWithCond
  let body = funcIfs
  return $ A.Function params body

convertLit = A.Lit . \case
  LitInt value -> A.LitInt value
  LitDouble value -> A.LitDouble value
  LitChar value -> A.LitChar value
  LitString value -> A.LitString value
  LitBool value -> A.LitBool value
