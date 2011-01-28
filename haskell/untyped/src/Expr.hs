module Expr where

import Control.Monad
import Data.Binary
import Env

data Binop = Add
           | Sub
           | Mul
           | Eq
           | Lt
           | Seq
           deriving Eq

instance Show Binop where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Eq  = "="
  show Lt  = "<"
  show Seq = ";"

instance Binary Binop where
  put Add = put (0 :: Word8)
  put Sub = put (1 :: Word8)
  put Mul = put (2 :: Word8)
  put Eq  = put (3 :: Word8)
  put Lt  = put (4 :: Word8)
  put Seq = put (5 :: Word8)
  get = do 
    tag <- getWord8
    case tag of
      0 -> return Add
      1 -> return Sub
      2 -> return Mul
      3 -> return Eq
      4 -> return Lt
      5 -> return Seq
      _ -> error "Decoding Binop"

data Expr = Lambda String Expr
          | Closure (Env Expr) String Expr
          | App Expr Expr
          | Var String
          | BoolVal Bool
          | NumVal Int
          | Binop Binop Expr Expr
          | If Expr Expr Expr
          | Print Expr
          | Error String
          deriving Eq

instance Binary Expr where
  put (Lambda x e)      = put (0 :: Word8) >> put x >> put e
  put (Closure env x e) = put (1 :: Word8) >> put env >> put x >> put e
  put (App e1 e2)       = put (2 :: Word8) >> put e1 >> put e2
  put (Var x)           = put (3 :: Word8) >> put x
  put (BoolVal b)       = put (4 :: Word8) >> put b
  put (NumVal n)        = put (5 :: Word8) >> put n
  put (Binop op e1 e2)  = put (6 :: Word8) >> put op >> put e1 >> put e2
  put (If e1 e2 e3)     = put (7 :: Word8) >> put e1 >> put e2 >> put e3
  put (Print e)         = put (8 :: Word8) >> put e
  put (Error msg)       = put (9 :: Word8) >> put msg
  get = do 
    tag <- getWord8
    case tag of
      0 -> liftM2 Lambda get get
      1 -> liftM3 Closure get get get
      2 -> liftM2 App get get
      3 -> liftM Var get
      4 -> liftM BoolVal get
      5 -> liftM NumVal get
      6 -> liftM3 Binop get get get
      7 -> liftM3 If get get get
      8 -> liftM Print get
      9 -> liftM Error get
      _ -> error "Decoding Expr"

instance Show Expr where
  show (Lambda x e) = "(\\" ++ x ++ "." ++ (show e) ++ ")"
  show (Closure _ x e) = "(\\" ++ x ++ "." ++ (show e) ++ ")"
  show (Var x) = x
  show (App e1 e2) = "(" ++ (show e1) ++ " $ " ++ (show e2) ++ ")"
  show (BoolVal True) = "true"
  show (BoolVal False) = "false"
  show (NumVal n) = show n
  show (Binop op lhs rhs) = "(" ++ (show lhs) ++ 
                                   (show op) ++ 
                                   (show rhs) ++ ")"
  show (If e1 e2 e3) = "(if " ++ (show e1) ++ 
                       " then " ++ (show e2) ++ 
                       " else " ++ (show e3) ++ ")"
  show (Print e) = "(print " ++ (show e) ++ ")"
  show (Error msg) = "(error: " ++ (show msg) ++ ")"
