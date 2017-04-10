module Oczor.Compiler.CommandLine where
import           ClassyPrelude
import           Oczor.Compiler.Compiler
import qualified Oczor.Compiler.State    as State
import           Oczor.Utl               hiding (argument)
import           Oczor.Version
import           Options.Applicative

data Options = Options
  { browse     :: Bool
  , lang       :: String
  , moduleName :: String
  , outputDir  :: String
  , srcDirs    :: [String]
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> switch browseParser
  <*> strOption languageParser
  <*> argument str (metavar "FILE")
  <*> strOption outputDirParser
  <*> many (strOption sourceDirsParser)
  where
    languageParser = long "lang" <> short 'l' <> value "js" <> showDefault <> metavar "LANGUAGE" <> help "target language (js, lua, rb, el)"
    outputDirParser = long "output" <> short 'o' <> value "output" <> showDefault <> metavar "DIRECTORY" <> help "output directory"
    browseParser = long "browse" <> short 'b' <> help "display the names defined by module"
    sourceDirsParser = long "src" <> short 's' <> metavar "DIRECTORIES..." <> help "source file directories"

optionsToState options = State.initState
  & State.showModule .~ browse options
  & State.lang .~ lang options
  & State.outputDir .~ outputDir options
  & State.srcDirs .~ srcDirs options
  & State.combine .~ True

runWith options = runCompilerPrint state compiler
  where
    state = optionsToState options
    compiler = compileAndWrite . fileToModuleName . moduleName $ options

run :: IO ()
run = execParser (info options description) >>= runWith
  where
    options = helper <*> optionsParser
    description = fullDesc <> progDesc desc <> header desc
    desc = unwords ["Oczor compiler", version]
