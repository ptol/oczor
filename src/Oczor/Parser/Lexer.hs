module Oczor.Parser.Lexer where

import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import ClassyPrelude hiding (try)
import Oczor.Parser.ParserState

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "/*" "*/"

scn :: Parser ()
scn = L.space (void spaceChar) lineComment blockComment

sc :: Parser ()
sc = L.space (void $ oneOf " ") lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol = L.symbol sc

identOut :: Parser String
identOut = lexeme $ char '^' *> some identChar

someIndent :: Parser a -> Parser [a]
someIndent p = do
  let sc' = scn
  level <- sc' *> L.indentLevel
  x     <- p
  xs    <- many (try $ L.indentGuard sc' EQ level *> p) <* sc'
  return (x:xs)

indentOrComma :: Parser a -> Parser [a]
indentOrComma p = try ( someIndent p) <|> commaSep1 p

litInt = lexeme L.integer
litFloat = lexeme L.float

litStr :: Parser String
litStr = lexeme $ char '"' *> manyTill L.charLiteral (char '"')

litChar :: Parser Char
litChar = lexeme $ char '\'' *> L.charLiteral <* char '\''

parens = between (symbol "(") (symbol ")")

brackets  = between (symbol "[") (symbol "]")

rword :: String -> Parser ()
rword w = string w *> notFollowedBy identChar *> sc

rop :: String -> Parser ()
rop w = string w *> notFollowedBy opChar *> sc

rws = ["if", "then", "else", "let", "with", "true", "false", "for", "include", "as", "in", "where", "infix", "infixl", "infixr", "prefix", "postfix", "type", "open", "import", "forall", "case", "class", "ffi", "match", "with"]

ros = ["=>", "@", "\\", ":", ":=", "<:", "|", "_", "=", "^"]

ident :: Parser String
ident = identWith lowerChar

opChar :: Parser Char
opChar = oneOf "!#$%&*+./<=>?@\\^|-~"

identChar :: Parser Char
identChar = alphaNumChar

identOp :: Parser String
identOp = lexeme $ some opChar

identType :: Parser String
identType = identWith upperChar

identWith x = identWith2 x identChar rws

identWith2 :: Parser Char -> Parser Char -> [String] -> Parser String
identWith2 x y z = (lexeme . try) (p >>= check)
  where
    p       = liftA2 (:) x (many y)
    check x = if x `elem` z
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

comma = symbol ","
semi = symbol ";"

commaSep1 p = p `sepBy1` comma
commaSep0 p = p `sepBy` comma

semiSep2 p = p `sepBy2` semi
semiSep1 p = p `sepBy1` semi

sepBy2 p sep = do
  x <- p
  _ <- sep
  y <- p
  xs <- many (sep >> p)
  return (x:y:xs)
