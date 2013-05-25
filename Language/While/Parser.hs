--------------------------------------------------------------------
-- |
-- Module : Language.While.Parser
--
-- Provides parsing of while-language code.
-- Supports reading either a file or stdin, resulting in an AST.

module Language.While.Parser (loadFile, loadStdin) where

import Control.Applicative ((<$>), (<*))
import Control.Monad (liftM)
import Language.While.Types
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Indent
import Text.Parsec.Language (GenLanguageDef)
import Text.Parsec.String
import qualified Text.Parsec.Token as P

-- | Parse the specified file and return either failure or the program.
loadFile :: FilePath -> IO (Either String Stm)
loadFile path = parseString "Failed to parse file; " <$> readFile path

-- | Parse stdin and return either failure or the program.
loadStdin :: IO (Either String Stm)
loadStdin = parseString "Failed to parse stdin; " <$> getContents

-- | Parse the supplied string and return either failure or the program.
parseString errMsg input =
  case parseResult of
    Left err -> Left $ errMsg ++ show err
    Right res -> Right res
  where
  parseResult = runIndent "" $ runParserT program () "" input

-- | Parse a binary operation.
binaryOp name fun = Infix body
  where
  body = reservedOp name >> return fun

-- | Parse a prefix operation, e.g. negation.
prefixOp name fun = Prefix $ reservedOp name >> return fun

-- | Parse a whole program, as a series of statements.
program = do
  whiteSpace
  st <- semiSep consecutive
  return $ foldr1 Scomp . concat $ st

-- | Parse a single statement followed optionally by another one.
-- | Needs to be done due to the while-statement.
consecutive = do
  s1 <- statement
  (s1:) <$> option [] (liftM return statement)

-- | Parse a program statement.
statement
  =   try stmAssignment
  <|> try stmTryCatch
  <|> try stmSkip
  <|> try stmIf
  <|> try stmWhile <* whiteSpace
  <|> parens stmWhile <* whiteSpace

-- | Parse an arithmetic atom.
arithmeticAtom
  =   Numeral <$> integer
  <|> Variable <$> identifier
  <|> parens arithmeticExpr

-- | Table of supported arithmetic operations.
arithmeticOperation =
  [ [binaryOp "*" Amul AssocLeft]
  , [binaryOp "+" Aadd AssocLeft, binaryOp "-" Asub AssocLeft , binaryOp "/" Adiv AssocLeft ]
  ]

-- Parse an arithmetic expression, consisting of parenthesis and supported operators.
arithmeticExpr = buildExpressionParser arithmeticOperation arithmeticAtom

-- | Since Bexp members operate over different domains,
--   there is some boxing/unboxing being done with WrapAtom.
data WrapAtom
  = BexpW Bexp
  | AexpW Aexp

-- | Parse a boolean atom.
booleanAtom
  =   (try (symbol "true") >> truthVal Btrue)
  <|> (try (symbol "false") >> truthVal Bfalse)
  <|> (try $ AexpW <$> arithmeticExpr)
  <|> parens booleanExpr'
  where truthVal = return . BexpW

-- | Table of supported boolean operations.
booleanOperation =
  [ [prefixOp "!" bneg]
  , [binaryOp "=" beq AssocLeft
   , binaryOp "<=" bleq AssocLeft
   , binaryOp "^" band AssocLeft ]
  ]
  where
  bneg (BexpW b) = BexpW $ Bneg b
  beq (AexpW a1) (AexpW a2) = BexpW $ Beq a1 a2
  bleq (AexpW a1) (AexpW a2) = BexpW $ Bleq a1 a2
  band (BexpW b1) (BexpW b2) = BexpW $ Band b1 b2

-- | Parse a boolean expression.
booleanExpr = do
  result <- booleanExpr'
  case result of
    (BexpW val) -> return val
    _ -> error "Parse error: failed to extract boolean"

booleanExpr' = buildExpressionParser booleanOperation booleanAtom

-- | Assignment statement.
stmAssignment = do
  var <- identifier
  symbol ":="
  expr <- arithmeticExpr
  return $ Sass var expr

-- | Skip statement.
stmSkip = symbol "skip" >> return Sskip

-- | If-then-else statement.
stmIf = do
  symbol "if"
  check <- booleanExpr
  symbol "then"
  s1 <- program
  symbol "else"
  s2 <- program
  return $ Sif check s1 s2

-- | While statement.
stmWhile = do
  symbol "while"
  check <- booleanExpr
  symbol "do"

  prog <- liftM (foldr1 Scomp) . block $ do
    s <- statement
    optional semi
    return s

  return $ Swhile check prog

-- | Try-catch statement.
stmTryCatch = do
  symbol "try"
  s1 <- program
  symbol "catch"
  s2 <- program
  return $ Stry s1 s2

identifier = P.identifier whileLexer
integer = P.integer whileLexer
parens = P.parens whileLexer
reservedOp = P.reservedOp whileLexer
semi = P.semi whileLexer
symbol = P.symbol whileLexer
semiSep = P.semiSep whileLexer
whiteSpace = P.whiteSpace whileLexer

whileLexer = P.makeTokenParser whileStyle

whileStyle :: Monad m => GenLanguageDef String u m
whileStyle = P.LanguageDef
  { P.commentStart   = ""
  , P.commentEnd     = ""
  , P.commentLine    = "#"
  , P.nestedComments = True
  , P.identStart     = letter <|> char '_'
  , P.identLetter    = alphaNum <|> oneOf "_'"
  , P.opStart        = P.opLetter whileStyle
  , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , P.reservedOpNames= []
  , P.reservedNames  = []
  , P.caseSensitive  = True
  }
