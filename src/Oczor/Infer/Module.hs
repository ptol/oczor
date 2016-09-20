{-# LANGUAGE TemplateHaskell #-}
module Oczor.Infer.Module where
import ClassyPrelude
import Oczor.Syntax.Syntax
import Oczor.Utl

type TypeName = String
type TypeClasses = Map String (String, TypeExpr)
type IsFfi = Bool

type Instances = Map TypeExpr [ClassName]

data Module = Module {
  _moduleName :: ModuleName,
  _idents :: Map String Scheme,
  _identsNs :: Map String ModuleName,
  _typeIdents :: Map String (TypeExpr, IsFfi),
  _instances :: Instances,
  _instancesType :: Map (TypeName, ClassName) TypeExpr,
  _classes :: TypeClasses,
  _operators :: OperatorGroups
} deriving (Eq, Show, Read)
makeLenses ''Module

rootModuleName :: ModuleName -> ModuleName
rootModuleName moduleName = moduleName ++ [lastEx moduleName]


getIdentNs ident mdl = mdl ^. identsNs & lookup ident & fromMaybe (mdl ^. moduleName)

mergeModules newMdl mdl = 
  mdl
    & idents %~ (\x -> unionMaps [x, newMdl ^. idents])
    & typeIdents %~ (\x -> unionMaps [x, newMdl ^. typeIdents])
    & instancesType %~ (\x -> unionMaps [x, newMdl ^. instancesType])
    & instances %~ (\x -> unionMaps [x, newMdl ^. instances])
    & classes %~ (\x -> unionMaps [x, newMdl ^. classes])
    & operators %~ (\x -> unionOperatorGroups x (newMdl ^. operators))
    & identsNs %~ (\x -> unionMaps [x, (newMdl ^. idents & keys) & map (\x -> (x, newMdl & getIdentNs x)) & mapFromList])


newModule name = Module {
  _moduleName = name,
  _idents = mempty,
  _identsNs = mempty,
  _classes = mempty,
  _typeIdents = mempty,
  _operators = mempty,
  _instances = mempty,
  _instancesType = mempty
}
