module Environment where

type Env a b = [(a,b)]

emptyEnv :: Eq a => [(a,b)]
emptyEnv = []

extend :: Eq a => [(a,b)] -> a -> b -> [(a,b)]
extend e x t = (x,t) : e

fetch :: Eq a => [(a,b)] -> a -> b
fetch e x = case lookup x e of Just t -> t
                               Nothing -> undefined
