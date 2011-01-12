module Interp (run) where

import Env (Env)
import qualified Env
import Expr

run :: Expr -> IO ()
run expr = do
  r <- eval Env.empty expr
  print r

eval :: Env Expr -> Expr -> IO Expr
eval env (Lambda x e) = return $ Closure env x e
eval _ (Closure env x e) = return $ Closure env x e
eval env (Var x) = do
  let e = Env.lookup x env
  eval env e
eval env (App e1 e2) = do
  e1' <- eval env e1
  case e1' of
    Closure cenv x e -> do
      arg <- eval env e2
      let env' = Env.extend x arg cenv
      eval env' e
    _ -> return Error
eval _ (BoolVal b) = return $ BoolVal b
eval _ (NumVal n) = return $ NumVal n
eval env (Binop op e1 e2) = do
  e1' <- eval env e1
  e2' <- eval env e2
  return $ doBinop op e1' e2'
eval env (If e1 e2 e3) = do
  e1' <- eval env e1
  case e1' of 
    BoolVal b -> if b then eval env e2 else eval env e3
    _ -> return Error
eval env (Print e) = do
  e' <- eval env e
  print e'
  return e'
eval _ Error = return Error

doBinop :: Binop -> Expr -> Expr -> Expr
doBinop Add (NumVal n1) (NumVal n2) = NumVal (n1 + n2)
doBinop Mult (NumVal n1) (NumVal n2) = NumVal (n1 * n2)
doBinop Eq e1 e2 = BoolVal (e1 == e2)
doBinop Seq _ e2 = e2
doBinop _ _ _ = Error
