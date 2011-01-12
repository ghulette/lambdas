module Env (Env,lookup,extend,empty,union,pretty) where

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as Map

type Env a = Map String a

lookup :: String -> Env a -> a
lookup x env = 
  case Map.lookup x env of
    Just e -> e
    Nothing -> error ("Not defined: " ++ x)

extend :: String -> a -> Env a -> Env a
extend = Map.insert

empty :: Env a
empty = Map.empty

union :: Env a -> Env a -> Env a
union = Map.union

pretty :: Show a => Env a -> String
pretty = unlines . map (\(x,e) -> x ++ " = " ++ (show e)) . Map.assocs
