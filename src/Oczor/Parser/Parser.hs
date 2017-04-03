module Oczor.Parser.Parser where

import Text.Megaparsec hiding (label)
import qualified Oczor.Parser.Lexer as L
import ClassyPrelude as C hiding (try)
import Oczor.Syntax.Syntax
import Control.Monad.State
import Oczor.Parser.ParserState
import Oczor.Parser.Utl
import Oczor.Parser.Expr
import Oczor.Parser.Statements
import Oczor.Parser.Types
import Oczor.Utl

recordItemTopLevel = stmt <|> recordItem

topLevelRecord :: Parser Expr
topLevelRecord = recordIndentWith recordItemTopLevel

parser :: Parser Expr
parser = topLevelRecord <* eof

parseAll :: Parser Expr -> String -> ModuleName -> OperatorGroups -> Either Error (Expr, OperatorGroups)
parseAll parser txt file opGroups =
  bimap 
    ((ParserError . parseErrorPretty) &&& getPosFromError)
    (exprListToRecord `bimap` (^. ops)) $
    parse (runStateT parser (emptyState & addOperators opGroups)) (moduleNameToIdent file) txt

parseExpr :: String -> Either Error Expr
parseExpr x = right fst $ parseAll parser x [] []

parset :: String -> IO ()
parset x = (putStrLn . pack) . either show (pshow . removeMD . fst) $ parseAll parser x [] []

parseType :: String -> Either Error Expr
parseType = parsew (ExprType <$> (typeRecord <* eof))

parsew :: Parser Expr -> String -> Either Error Expr
parsew p x = removeMD . fst <$> parseAll p x [] []

getImports :: String -> Either Error [ModuleName]
getImports x =
  parsew ( many (stmtImport <* L.scn) <&> recordIfSome) x & map (\x -> recordToList x & map (\(Stmt (StmtImport x Nothing)) -> x))
