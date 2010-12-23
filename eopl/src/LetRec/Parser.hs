module LetRec.Parser where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Expr as Ex
import LetRec.Lexer
import LetRec.Expr

var :: Parser Expr
var = do
  x <- identifier
  return (Var x)

constNum :: Parser Expr
constNum = do
  n <- natural
  return (Const (fromIntegral n))

letIn :: Parser Expr
letIn = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return (Let x e1 e2)

letrecIn :: Parser Expr
letrecIn = do
  reserved "letrec"
  fx <- identifier
  x <- parens identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return (LetRec fx x e1 e2)

ifThenElse :: Parser Expr
ifThenElse = do
  reserved "if"
  e1 <- expr
  reserved "then"
  e2 <- expr
  reserved "else"
  e3 <- expr
  return (If e1 e2 e3)

procDef :: Parser Expr
procDef = do
  reserved "proc"
  x <- parens identifier
  e <- expr
  return (Proc x e)

procCall :: Parser Expr
procCall = do
  e1 <- expr
  e2 <- expr
  return (Call e1 e2)

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor
  where prefixOp x f = Ex.Prefix (reservedOp x >> return f)
        infixOp x f = Ex.Infix (reservedOp x >> return f)
        table = [[infixOp "-" Diff Ex.AssocLeft],
                 [prefixOp "zero?" IsZero]]
        factor =  try (parens expr)
              <|> try (parens procCall)
              <|> var
              <|> constNum
              <|> ifThenElse
              <|> letIn
              <|> letrecIn
              <|> procDef
              <?> "factor"

-- Wrappers

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (allOf expr) "LetRec"
