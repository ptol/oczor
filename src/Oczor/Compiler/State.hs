{-# LANGUAGE TemplateHaskell #-}
module Oczor.Compiler.State where

import ClassyPrelude
import Oczor.Infer.InferContext
import Control.Monad.State
import Control.Monad.Except
import Oczor.Syntax.Syntax

import Oczor.Utl

type Lang = String
type LangSrc = String
type OcSrc = String
type OcWithFfi = (Maybe String, OcSrc)

data CompState = CompState {
  _lang :: String,
  _combine :: Bool,
  _loadModules :: Modules,
  _modulesLangSrc :: Map ModuleName LangSrc,
  _modulesOrder :: [ModuleName],
  _srcDirs :: [FilePath],
  _outputDir :: FilePath,
  _showModule :: Bool,
  _compilingModules :: [ModuleName]
}

makeLenses ''CompState

initState = CompState {
  _lang = "js",
  _combine = False,
  _loadModules = mempty,
  _modulesLangSrc = mempty,
  _modulesOrder = mempty,
  _srcDirs = ["src/"],
  _showModule = False,
  _outputDir = "output",
  _compilingModules = mempty
}


type Compiler a = StateT CompState (ExceptT Error IO) a

