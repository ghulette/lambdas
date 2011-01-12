module Parser (parseExpr) where

import Control.Monad.Instances ()
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Expr as Ex
import Lexer
import Expr

variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)

literalNum :: Parser Expr
literalNum = do
  n <- natural
  return (NumVal (fromIntegral n))

literalTrue :: Parser Expr
literalTrue = do
  reserved "true"
  return (BoolVal True)

literalFalse :: Parser Expr
literalFalse = do
  reserved "false"
  return (BoolVal False)

ifthenelse :: Parser Expr
ifthenelse = do
  reserved "if"
  e1 <- expr
  reserved "then"
  e2 <- expr
  reserved "else"
  e3 <- expr
  return (If e1 e2 e3)

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  x <- identifier
  reservedOp "."
  e <- expr
  return (Lambda x e)

letin :: Parser Expr
letin = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return (App (Lambda x e2) e1)

printExpr :: Parser Expr
printExpr = do
  reserved "print"
  e <- expr
  return (Print e)

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor
  where infixOp x f = Ex.Infix (reservedOp x >> return f)
        table = [[infixOp ";" (Binop Seq) Ex.AssocLeft],
                 [infixOp "$" App Ex.AssocLeft],
                 [infixOp "*" (Binop Mult) Ex.AssocLeft],
                 [infixOp "+" (Binop Add) Ex.AssocLeft],
                 [infixOp "=" (Binop Eq) Ex.AssocLeft]]
        factor =  parens expr
              <|> variable
              <|> literalTrue
              <|> literalFalse
              <|> literalNum
              <|> ifthenelse
              <|> lambda
              <|> letin
              <|> printExpr
              <?> "factor"

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (allOf expr) "Expr"
