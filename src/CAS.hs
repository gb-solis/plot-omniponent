module CAS where

import           Control.Monad             (ap, liftM, liftM2)

data Term a = Const a | Var deriving Eq

instance Show a => Show (Term a) where
    show (Const a) = show a
    show Var       = "x"

data Expr a = Sum (Expr a) (Expr a)
            | Prod (Expr a) (Expr a)
            | Exp (Expr a)
            | Leaf a
            deriving Eq

-- | Generate all expressions up to height n with bottom specified leaves
genUpTo :: [a] -> Int -> [Expr a]
genUpTo leaves 0 = map Leaf leaves
genUpTo leaves n = shallowExprs ++ binaryExprs ++ unaryExprs where
    shallowExprs = genUpTo leaves (n-1)
    binaryExprs = concatMap (\op -> map (uncurry op) $ liftM2 (,) shallowExprs shallowExprs) [Sum, Prod] -- Can be optimized for commmutative operators
    unaryExprs = concatMap (`map` shallowExprs) [Exp]

instance Show a => Show (Expr a) where
    show (Sum e1 e2)  = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Prod e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
    show (Exp e)      = "exp(" ++ show e ++ ")"
    show (Leaf a)     = show a

instance Functor Expr where
    fmap = liftM

instance Applicative Expr where
    pure = Leaf
    (<*>) = ap

instance Monad Expr where
    (Sum e1 e2)  >>= f = Sum (e1 >>= f) (e2 >>= f)
    (Prod e1 e2) >>= f = Prod (e1 >>= f) (e2 >>= f)
    (Exp e)      >>= f = Exp (e >>= f)
    (Leaf a)     >>= f = f a

type SymExpr = Expr (Term Float)

type NumExpr = Expr Float

eval :: NumExpr -> Float
eval (Sum a b)  = eval a + eval b
eval (Prod a b) = eval a * eval b
eval (Exp e)    = exp (eval e)
eval (Leaf a)   = a

subs :: Expr (Term a) -> a -> Expr a
subs e a =  fmap substitute e where
    substitute (Const b) = b
    substitute Var       = a

($|) :: SymExpr -> Float -> Float
e $| a = eval $ subs e a

-- | Numerically evaluates the distance between the two functions in the L^2([a,b]) norm
-- within a certain interval with interpaced x's by epsilon
l2dist :: (Float, Float) -> Float -> SymExpr -> SymExpr -> Float
l2dist (a, b) epsilon e1 e2 = sum $ map integrand [a, a+epsilon..b] where
    integrand x = epsilon * (e1 $| x - e2 $| x)**2
