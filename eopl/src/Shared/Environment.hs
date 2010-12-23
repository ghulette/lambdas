module Shared.Environment where

import Data.Maybe (fromJust)

type Env a = [(String,a)]

initEnv :: Env a
initEnv = []

applyEnv :: Env a -> String -> a
applyEnv e x = fromJust (lookup x e)

extendEnv :: String -> a -> Env a -> Env a
extendEnv x t = (:) (x,t)
