module Oczor.Infer.Unify where

import Oczor.Syntax.Syntax
import Oczor.Utl
import Control.Monad.Except
import Control.Monad.RWS
import ClassyPrelude as C hiding (TVar)
import Oczor.Infer.InferContext
import Oczor.Infer.State
import Oczor.Infer.Substitutable
import Oczor.Infer.UnifyState

-- runUnify x y |  traceResult ["=== runUnify ===", show x, show y] = trac "=== result ===" <$> runReaderT (unify x y) initState
runUnify x y = runReaderT (unify x y) initState

unify :: TypeExpr -> TypeExpr -> Unify Subst
-- unify arg param | traceArgs ["unify", show arg, show param] = undefined
unify arg param = do
  context <- lift ask
  state <- ask
  case (arg,param) of
    _ | arg == param -> return emptySubst
    (arg, TypeConstraints list expr) ->
      let newContext = state &addConstraints list in addConstraintsToSubst newContext <$> localPut newContext (unify arg expr)
    (TypeConstraints list expr, param) ->
      let newContext = state &addConstraints list in addConstraintsToSubst newContext <$> localPut newContext (unify expr param)

    (TypeApply argType@(TypeIdent argIdent) argBody, TypeApply paramType@(TypeIdent paramIdent) paramBody) |
      argIdent == paramIdent && isFfiType argType context && isFfiType paramType context -> unifyList argBody paramBody
    (_,_) | Just argIdent <- getTypeIdent arg, Just paramIdent <- getTypeIdent param,
      argIdent == paramIdent && isFfiTypeIdent argIdent context && isFfiTypeIdent paramIdent context -> return emptySubst

    (TypeUnion argList, TypeUnion _) -> do
      ys <- traverse (\x -> unify x param) argList
      return $ ys & composeSubstList

    (arg, TypeUnion paramList) -> do
      ys <- catMaybes <$> mapM (orNothing . unify arg) paramList
      let r = ys & sortOn (\(Subst x) -> length x) & headMay
      maybe (typeErrorLift $ UnificationFail arg param) return r

    (arg, param) | Just tupleType <- getTupleType arg, Just typeList <- getTypeList param -> unifyList tupleType typeList
    (arg, param) | Just tupleType <- getTupleType param, Just typeList <- getTypeList arg -> unifyList typeList tupleType 

    (TypeRecord argList, param) | length argList == 1, Just h <- headMay argList -> unify h param
    (arg, TypeRecord paramList)| length paramList == 1, Just h <- headMay paramList -> unify arg h
    (TypeRecord argList, TypeRecord paramList) -> unifyList argList paramList

    (TypeRecord argList, TypeRow x paramList) -> unifyList argList paramList
    (TypeRow x argList, TypeRecord paramList) -> unifyList argList paramList

    (TypeRow (TypeVar a) argList, TypeRow (TypeVar b) paramList) -> do
      fv <- lift freshVar
      (s,t) <- unifySubTypes context argList paramList
      let r = TypeRow (TypeVar fv) t
      return $ composeSubst (Subst $ mapFromList [(a, r), (b, r)]) s

    (TypeLabel name1 expr1, TypeLabel name2 expr2) | name1 == name2 -> unify expr1 expr2

    (TypeFunc in1 out1, TypeFunc in2 out2) -> unifyList [in1, out1] [in2, out2]

    (TypeVar var, param) | notNull $ getConstraints var state -> bindWithConstraints var param
    (arg, TypeVar var) | notNull $ getConstraints var state -> bindWithConstraints var arg

    (TypeApply x arga, TypeApply y argp) -> do
      let (newArg, newParam) = curryTypeApply2 arg param
      unifyApply False newArg newParam

    (TypeVar var, param) -> bind var param
    (arg, TypeVar var) -> bind var arg

    (TypeUnion argList, param ) -> do
      x <- orNothing $ unifyList argList (replicate (length argList) param)
      maybe (typeErrorLift $ UnificationFail arg param) return x

    (arg, TypeApply ti@(TypeIdent _) _) | not $ isFfiType ti context -> unifyApply False arg param
    (TypeApply ti@(TypeIdent _) _, param )| not $ isFfiType ti context -> unifyApply True param arg

    (TypePoly in1 out1, TypePoly in2 out2) | eqLength in1 in2 -> unifyList (in1 ++ [out1]) (in2 ++ [out2])

    (arg, TypeIdent _) | not $ isFfiType param context -> unifyIdent True param arg
    (arg, TypePoly in2 out2) -> unify arg out2

    (TypeIdent _, param) | not $ isFfiType arg context -> unifyIdent False arg param
    (TypePoly in1 out1, param) -> unify out1 param


    (TypeRecord argList, param) -> unifyList argList [param]

    (TypeRow x argList, param) -> unify (TypeRecord argList) param
    (arg, TypeRow x paramList)-> unify (TypeRecord paramList) arg

    (TypeLabel name1 expr1, param) | isTupleLabel name1 -> unify expr1 param
    (arg, param @(TypeLabel name1 expr1))| isTupleLabel name1 -> unify arg expr1
    _ -> typeErrorLift $ UnificationFail arg param

getTypeList = \case
  TypeRecord x -> Just x
  TypeRow _ x -> Just x
  _ -> Nothing

getTupleType = \case
  (TypeRecord x) | length x > 1 -> x <&> getTupleLabelType & sequence
  (TypeRow _ x) | length x > 1 -> x <&> getTupleLabelType & sequence
  _ -> Nothing

addConstraintToExpr :: ConstraintSet -> TypeExpr -> TypeExpr
addConstraintToExpr set = cata $ \case
  ast@(TypeVarF var) ->
    let a = Fix ast in
      if onull constraints then a
      else TypeConstraints (constraintListToSet var constraints) a
    where
      constraints = setGetConstraints var set
  x -> Fix x

addConstraintsToSubst context s@(Subst subst) =
  if null cList then s
  else Subst $ map (addConstraintToExpr cList) subst
  where
    cList = context ^. constraints

-- hasInstance context x varConstrain | traceArgs ["hasInstance", show x, show varConstrain, show $ context ^. instances] = undefined
hasInstance x varConstrain = do
  context <- lift ask
  let instances = getInstancesForConstrain context varConstrain
  r <- orNothing $ unify x (TypeUnion instances)
  maybe (typeErrorLift $ NoInstance x varConstrain) (return . return) r
hasInstances x varConstraints  = do
  x <- varConstraints &traverse (hasInstance x)
  return $ composeSubstList <$> x

constraintListToSet :: String -> [String] -> ConstraintSet
constraintListToSet var all = all & map (\x -> (TypeVar var,x))

newTypeConstraints var all =
  let tv = TypeVar var in
    if onull all then tv else TypeConstraints (constraintListToSet var all) tv

combineTypeVarConstraints context var1 var2 = all
  where
    var1Constraints = getConstraints var1 context
    var2Constraints = getConstraints var2 context
    all = var1Constraints ++ var2Constraints & ordNub

bindWithConstraints :: TVar -> TypeExpr -> Unify Subst
-- bindWithConstraints context var x | traceArgs ["bindWithConstraints", show $ context ^. constraints, show var, show x] = undefined
bindWithConstraints var x@(TypeVar vx) = do
  state <- ask
  fv <- lift freshVar
  let allConst = combineTypeVarConstraints state var vx
  let newTypeVar = newTypeConstraints fv allConst
  return $ Subst $ mapFromList [(var, newTypeVar), (vx, newTypeVar)] 
bindWithConstraints var x = do
  state <- ask
  let varConstraints = getConstraints var state
  hasInstances x varConstraints *> bind var x --TODO check *>

bind ::TVar -> TypeExpr -> Unify Subst
-- bind context var x | traceArgs ["bind", show var, show x] = undefined
bind a t | t == TypeVar a     = return emptySubst
         | occursCheck a t = typeErrorLift $ InfiniteType a t
         | otherwise       = return (Subst $ singletonMap a t)
    where
    occursCheck ::  Substitutable a => TVar -> a -> Bool
    occursCheck a t = member a (ftv t)

orNothing :: MonadError e m => m a -> m (Maybe a)
orNothing p = catchError (fmap return p) (\e -> return Nothing)
orEither :: MonadError a1 m => m a -> m (Either a1 a)
orEither p = catchError (return <$> p) (return . Left)


unifySubTypes context argList paramList = (,l) <$> unifyList x1 x2 where
   l = ordNub (argList ++ paramList)
   (x1,x2) = l & filter isTypeLabel & groupMapBy (\(TypeLabel label _) -> label)
    & filter (\ (_, y) -> olength y >= 2) & map (\(_, [x,y]) -> (x,y)) & unzip

unifyIdent :: Bool -> TypeExpr -> TypeExpr -> Unify Subst
unifyIdent switch arg@(TypeIdent argIdent) param = do
  (newState,argTypeDecl) <- getType argIdent
  localPut newState (if switch then unify param argTypeDecl else unify argTypeDecl param)


unifyApply :: Bool -> TypeExpr -> TypeExpr -> Unify Subst
-- unifyApply switch context arg param | traceArgs ["unifyApply", show arg, show param] = undefined
unifyApply switch arg@(TypeApply x applyArg1) param@(TypeApply y applyArg2) | eqLength applyArg1 applyArg2 =
  unifyList (x : applyArg1) (y : applyArg2)
unifyApply switch arg param@(TypeApply (TypeIdent ident) applyArg) = do
  (newState, typeMaker) <- getType ident
  let paramType = makeType typeMaker applyArg
  if switch then unify paramType arg else unify arg paramType

unifyApply switch x y = error $ unwords ["unifyApply", show x, show y]


unifyList :: [TypeExpr] -> [TypeExpr] -> Unify Subst
-- unifyList x y | traceArgs ["unifyList", show x, show y] = undefined
unifyList [TypeVar _] [] = return emptySubst
unifyList [] [] = return emptySubst
unifyList (t1 : ts1) (t2 : ts2) =
  do su1 <- unify t1 t2
     su2 <- unifyList (apply su1 ts1) (apply su1 ts2)
     return (su2 `composeSubst` su1)
unifyList t1 t2 = typeErrorLift $ UnificationMismatch t1 t2

typeErrorLift x = lift $ typeError x
typeError :: ErrorType -> Infer a
typeError er = do
  ctx <- ask
  let pos = ctx ^. position & fromMaybe (0,0,"no-file") -- TODO remove no-file
  throwError (er, pos)

getType :: String -> Unify (UnifyState, TypeExpr)
-- getType name | traceArgs ["getType", show name] = undefined
getType name = do
  state <- ask
  ctx <- lift ask
  lift $ fromMaybe (typeError $ UnboundType name) $ do 
    (tp, isFfi) <- ctx & lookupType name
    if isFfi then return $ typeError (TextError $ unwords ["type", show name, "is ffi"])
    else if state ^. openTypes & member name then return $ typeError (TextError $ unwords ["type synonym", show name, "is already used"])
    else
        return ((,) <$> updateContext state tp <*> return tp)
    where
      updateContext :: UnifyState -> TypeExpr -> Infer UnifyState
      updateContext state x =
        let openIdents = setFromList $ getOpenIdents x in
          if intersection openIdents (state ^. openTypes) & onull then
            return $ state & openTypes %~ union openIdents
          else
            typeError (TextError $ unwords ["type", show x, "is already opened"])
      getOpenIdents (TypeIdent x)  = [x]
      getOpenIdents (TypeUnion list)  = foldMap getOpenIdents list
      getOpenIdents _ = []
