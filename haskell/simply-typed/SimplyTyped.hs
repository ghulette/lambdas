import Environment

type Id = String

type Gamma a = Env Id a

data Type = NatT
          | AbsT Type Type
          | ProdT Type Type
          deriving Eq
          
instance Show Type where
  show NatT = "nat"
  show (AbsT t1 t2) = "(" ++ (show t1) ++ " -> " ++ (show t2) ++ ")"
  show (ProdT t1 t2) = "(" ++ (show t1) ++ "," ++ (show t2) ++ ")"

data Term = Abs Id Type Term
          | Closure (Gamma Term) Id Term
          | App Term Term
          | Var Id
          | Product Term Term
          | Fst Term
          | Snd Term
          | Zero
          | Succ Term
          deriving Eq

instance Show Term where
  show (Abs x k t) = "(\\" ++ x ++ ":" ++ (show k) ++ "." ++ (show t) ++ ")"
  show (Closure e x t) = show t
  show (App t1 t2) = "(" ++ (show t1) ++ " " ++ (show t2) ++ ")"
  show (Var x) = x
  show (Product t1 t2) = "(" ++ (show t1) ++ "," ++ (show t2) ++ ")"
  show (Fst t) = "(fst " ++ (show t) ++ ")"
  show (Snd t) = "(snd " ++ (show t) ++ ")"
  show (Zero) = "0"
  show (Succ t) = "(succ " ++ (show t) ++ ")"


-- Dynamic evaluation

eval' :: Gamma Term -> Term -> Term
eval' e (Abs x k t) = Closure e x t
eval' e (App t1 t2) = 
  case eval' e t1 of Closure e' x t1' -> eval' (extend e' x t2) t1'
                     otherwise -> undefined
eval' e (Var x) = eval' e (fetch e x)
eval' e (Fst (Product t1 _)) = eval' e t1
eval' e (Snd (Product _ t2)) = eval' e t2
eval' e (Product t1 t2) = Product t1' t2'
  where t1' = eval' e t1
        t2' = eval' e t2
eval' e Zero = Zero
eval' e (Succ t) = Succ (eval' e t)
eval' _ _ = undefined

eval :: Term -> Term
eval t = eval' emptyEnv t


-- Static evaluation

typeof' :: Gamma Type -> Term -> Type
typeof' g (Abs x k t) = AbsT k (typeof' (extend g x k) t)
typeof' g (App t1 t2) = 
  let j = typeof' g t2 in
  case typeof' g t1 of AbsT j' k | j == j' -> k
                       otherwise -> undefined
typeof' g (Var x) = fetch g x
typeof' g (Product t1 t2) = ProdT (typeof' g t1) (typeof' g t2)
typeof' g (Fst t) = case typeof' g t of ProdT k1 _ -> k1
                                        otherwise -> undefined
typeof' g (Snd t) = case typeof' g t of ProdT _ k2 -> k2
                                        otherwise -> undefined
typeof' g (Zero) = NatT
typeof' g (Succ t) = case typeof' g t of NatT -> NatT
                                         otherwise -> undefined
typeof' g (Closure _ _ _) = undefined


typeof :: Term -> Type
typeof = typeof' emptyEnv

run :: Term -> IO ()
run t = do
  let k = typeof t
  let r = eval t
  putStrLn $ (show r) ++ " : " ++ (show k)

main :: IO ()
main = do
  let x = "x"
  let y = "y"
  let p1 = Abs y NatT (Var y)
  let p2 = Abs x (AbsT NatT NatT) (Product (Var x) (Var x))
  let p3 = App p2 p1
  let p4 = App p2 (Abs x NatT (Succ (Var x)))
  mapM_ run [p1,p2,p3,p4]
