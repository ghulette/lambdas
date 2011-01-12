module LetRec.Expr where

import Shared.Environment

type Proc = (String,Expr,Env Val)

applyProc :: Proc -> Val -> Val
applyProc (x,e,env) v =
  valueOf e (extendEnv x v env)

data Val = NumVal Int
         | BoolVal Bool
         | ProcVal Proc
         deriving (Eq,Show)

numVal :: Val -> Int
numVal (NumVal n) = n
numVal _ = error "Value is not a number"

boolVal :: Val -> Bool
boolVal (BoolVal b) = b
boolVal _ = error "Value is not a boolean"

procVal :: Val -> Proc
procVal (ProcVal p) = p
procVal _ = error "Value is not a proc"

data Expr = Const Int
          | Var String
          | Diff Expr Expr
          | IsZero Expr
          | If Expr Expr Expr
          | Let String Expr Expr
          | Proc String Expr
          | Call Expr Expr
          | LetRec String String Expr Expr
          deriving (Eq,Show)

valueOf :: Expr -> Env Val -> Val
valueOf (Const n) _ = NumVal n
valueOf (Var x) env = applyEnv env x
valueOf (Diff e1 e2) env = 
  let n1 = numVal (valueOf e1 env)
      n2 = numVal (valueOf e2 env)
  in NumVal (n1 - n2)
valueOf (IsZero e) env = 
  let n = numVal (valueOf e env) 
  in BoolVal (n == 0)
valueOf (If e1 e2 e3) env =
  if boolVal (valueOf e1 env) 
    then valueOf e2 env
    else valueOf e3 env
valueOf (Let x e1 e2) env =
  let v1 = valueOf e1 env
  in valueOf e2 (extendEnv x v1 env)
valueOf (LetRec fx x e1 e2) env = 
  -- Instead of using the rather obtuse impl in EOPL, exploit Haskell's
  -- non-strict evaluation to implement recursion.
  let env' = extendEnv fx p env
      p = ProcVal (x,e1,env')
  in valueOf e2 env'
valueOf (Proc x e) env = ProcVal (x,e,env)
valueOf (Call e1 e2) env = 
  let proc = procVal (valueOf e1 env)
      arg = valueOf e2 env
  in applyProc proc arg

valueOfProgram :: Expr -> Val
valueOfProgram e = valueOf e initEnv
