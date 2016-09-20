module Oczor.Compiler.CommandLine where
import ClassyPrelude hiding ((<>))
import Options.Applicative
import Oczor.Compiler.Compiler
import Oczor.Compiler.State
import Oczor.Utl hiding (argument)

version = "0.0.1"

data Options = Options
  { lng :: String
  , output :: String
  , showMdl :: Bool
  , srcDirList :: [String]
  , moduleName :: String}

options :: Parser Options
options = Options
  <$> strOption ( long "lang" <> short 'l' <> value "js" <> showDefault <> metavar "LANGUAGE" <> help "target language (js, lua, rb, el)")
  <*> strOption ( long "output" <> short 'o' <> value "output" <> showDefault <> metavar "DIRECTORY" <> help "output directory" )
  <*> switch ( long "browse" <> short 'b' <> help "display the names defined by module" )
  <*> many (strOption ( long "src" <> short 's'  <> metavar "DIRECTORIES..." <> help "source file directories" ))
  <*> argument str (metavar "FILE")

optionsToState (Options lng output showMdl srcDirList moduleName) = initState
  & outputDir .~ output
  & srcDirs .~ srcDirList
  & showModule .~ showMdl
  & combine .~ True
  & lang .~ lng

runWith :: Options -> IO ()
runWith x@Options {} = runCompilerPrint (optionsToState x) $ compileAndWrite (fileToModuleName (moduleName x))

desc = unwords ["Oczor compiler", version]
run :: IO ()
run = execParser opts >>= runWith
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc desc
     <> header desc )
