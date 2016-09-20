{-# LANGUAGE TemplateHaskell #-}
module Oczor.Infer.InferContext (module Oczor.Infer.InferContext, module Oczor.Infer.Module) where

import Oczor.Syntax.Syntax
import ClassyPrelude 
import Control.Lens
import Oczor.Utl
import Oczor.Infer.Substitutable
import Oczor.Infer.Module

type VarName = String
type Types = Map String TypeExpr
type Rules = Map String TypeExpr
type InstanceType = String

type Modules = Map ModuleName Module

data InferContext = InferContext {
  _cmodule :: Module,
  _openModules :: Module,
  _modules :: Modules,
  _params :: Set String,
  _position :: Maybe AstPosition
} deriving (Eq, Show)

makeLenses ''InferContext


allOperators ctx = ctx ^. modules & map (view operators) & foldl' unionOperatorGroups []
allInstances ctx = (ctx ^. cmodule . instances &mapToList) ++ (ctx ^. openModules . instances &mapToList)

moduleLookup getter name ctx =
  (ctx ^. cmodule . getter & lookup name) <|> (ctx ^. openModules . getter & lookup name)


addOpenModule :: ModuleName -> Module -> InferContext -> InferContext
-- addOpenModule name newMdl ctx | traceArgs ["addOpenModule", show name, pshow newMdl] = undefined
addOpenModule name newMdl ctx =
    ctx
      & openModules %~ mergeModules newMdl
      -- & openModules %~ identsNs %~ (\x -> unionMaps [x, (newMdl ^. idents & keys) & map (\x -> (x, newMdl & getIdentNs x)) & mapFromList])

includeModule name newMdl ctx =
    ctx
      & cmodule %~ mergeModules newMdl
      -- & cmodule %~ (identsNs %~ (\x -> unionMaps [x, (newMdl ^. idents & keys) & map (\x -> (x, newMdl & getIdentNs x)) & mapFromList])) 


lookupModule name ctx = let mdls = ctx ^. modules in (mdls & lookup name) <|> (mdls & lookup (rootModuleName name))

getModuleSchemeFromContext mdl = Forall (vars & concat & ordNub) (newTypeRecord (labels &map (uncurry TypeLabel)))
  where
    (vars, labels) = mdl ^. idents & mapToList & map (\(name, Forall vars t) -> (vars, (name, t))) & unzip

importModuleToContext c1 name newModule =
  c1
    & cmodule . idents %~ insertMap name moduleScheme
  where
    moduleScheme = getModuleSchemeFromContext newModule

emptyContext = InferContext {
  _cmodule = newModule [],
  _openModules = newModule [],
  _params = mempty,
  _modules = mempty,
  _position = Nothing
}


getInstances tp context = context & moduleLookup instances tp &fromMaybe []

lookupClass name context = context & moduleLookup classes name

getIdentType context name = context & moduleLookup idents name

getInstancesForConstrain :: InferContext -> String -> [TypeExpr]
getInstancesForConstrain context cls = context & allInstances &filter(\(tp, clss) -> elem cls clss) &map fst

-- getTypeDecl context name = context ^. types &lookup name
addTypeDecl name tp isFfi context = context & cmodule . typeIdents %~ insertMap name (tp, isFfi)

lookupType :: String -> InferContext -> Maybe (TypeExpr, IsFfi)
lookupType name context = context & moduleLookup typeIdents name

lookupInstanceType typeIdent cls context = context & moduleLookup instancesType (typeIdent, cls)

baseTypes = ["Int", "Array", "Double", "Unit", "String", "Char", "Bool"] -- &map TypeIdent

-- ffis = [Ffi "true" typeBool, FFi "false" typeBool, Ffi "emptyObject" (TypeVar "a"), Ffi "eqAny" (TypeFunc (TypeRecord (TypeVar "a")))]

baseTypeContext = foldl' (\a e -> addTypeDecl e (TypeIdent e) True a) emptyContext baseTypes

isFfiTypeIdent ident context = context & lookupType ident & maybe False snd
isFfiType (TypeIdent ident) context = context & isFfiTypeIdent ident
isFfiType _ _ = False


instance Substitutable InferContext where
  apply s context = (context) & cmodule . idents %~ (map (apply s))
  ftv context = (ftv $ (context ^. cmodule . idents) & mapToList <&> snd)
