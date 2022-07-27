module CAS where

import           Control.Monad             (ap, liftM, liftM2)
import           Data.Functor.Identity     (Identity)
import           Data.Char                 (isSpace)
import           Text.Parsec               (Parsec, ParseError, parse, char, between, string, (<|>), option, digit, many1, oneOf, eof)
import           Text.Parsec.Expr          (Assoc(..), Operator(..), buildExpressionParser)
import           Data.Functor              (($>))
import           Data.List                 (groupBy)

data Term a = Const a | Var deriving Eq

instance Show a => Show (Term a) where
    show (Const a) = show a
    show Var       = "x"

-- | All binary operators in order of precedence. They are all associative
data BinaryOp = Prod | Sum deriving (Eq, Bounded, Enum)

instance Show BinaryOp where
    show Prod = "*"
    show Sum = "+"

precedence :: BinaryOp -> Int
precedence Sum  = 1
precedence Prod = 2

binaryFunction :: BinaryOp -> Float -> Float -> Float
binaryFunction Prod = (*)
binaryFunction Sum  = (+)

data UnaryOp = Exp deriving (Eq, Bounded, Enum)

instance Show UnaryOp where
    show Exp = "exp"

unaryFunction :: UnaryOp ->  Float -> Float
unaryFunction Exp = exp

data Expr a = Binary BinaryOp (Expr a) (Expr a)
            | Unary UnaryOp (Expr a)
            | Leaf a
            deriving Eq

instance Show a => Show (Expr a) where
    show (Binary bOpOuter e1 e2) = showParens e1 ++ show bOpOuter ++ showParens e2 where
        showParens e@(Binary bOpIn _ _) =
            if precedence bOpOuter >= precedence bOpIn && bOpOuter /= bOpIn
                then "(" ++ show e ++ ")"
                else show e
        showParens e = show e
    show (Unary uOp e) = show uOp ++ "(" ++ show e ++ ")"
    show (Leaf a)      = show a

instance Functor Expr where
    fmap = liftM

instance Applicative Expr where
    pure = Leaf
    (<*>) = ap

instance Monad Expr where
    (Binary bOp e1 e2) >>= f = Binary bOp (e1 >>= f) (e2 >>= f)
    (Unary uOp e)      >>= f = Unary uOp (e >>= f)
    (Leaf a)           >>= f = f a

-- | Generate all expressions up to height n with bottom specified leaves
genUpTo :: [a] -> Int -> [Expr a]
genUpTo leaves 0 = map Leaf leaves
genUpTo leaves n = shallowExprs ++ binaryExprs ++ unaryExprs where
    shallowExprs = genUpTo leaves (n-1)
    allBinaryOps = map Binary $ enumFrom minBound
    allUnaryOps = map Unary $ enumFrom minBound
    binaryExprs = allBinaryOps <*> shallowExprs <*> shallowExprs -- Can be optimized for commmutative operators
    unaryExprs  = allUnaryOps  <*> shallowExprs


type SymExpr = Expr (Term Float)

termParser :: Parsec String () (Term Float)
termParser = varP <|> constP where
    constP = Const <$> float
    varP = string (show (Var :: Term Float)) $> Var
    -- Below are helper functions to parse Floats.
    -- Source: https://www.schoolofhaskell.com/user/stevely/parsing-floats-with-parsec
    (<++>) a b = (++) <$> a <*> b
    (<:>) a b = (:) <$> a <*> b
    number = many1 digit
    plus = char '+' *> number
    minus = char '-' <:> number
    integer = plus <|> minus <|> number
    float = fmap rd $ integer <++> decimal <++> expt
        where rd       = read :: String -> Float
              decimal  = option "" $ char '.' <:> number
              expt = option "" $ oneOf "eE" <:> integer

symExprParser :: Parsec String () SymExpr
symExprParser = buildExpressionParser tableP termP where
    parensP = between (char '(') (char ')')
    termP = parensP symExprParser <|> (Leaf <$> termParser)
    tableP = map prefix (enumFrom minBound :: [UnaryOp])
           : map (map binaryLeft) (groupBy (\a b -> (precedence a) == (precedence b)) $ enumFrom minBound)
    binaryLeft bOp = Infix (string (show bOp) $> Binary bOp) AssocLeft :: Operator String () Identity SymExpr
    prefix uOp     = Prefix (string (show uOp) $> Unary uOp)      :: Operator String () Identity SymExpr

parseSymExpr :: String -> Either ParseError SymExpr
parseSymExpr = parse (symExprParser <* eof) "" . filter (not . isSpace)

readSymExpr :: String -> SymExpr
readSymExpr = either (error . show) id . parseSymExpr

type NumExpr = Expr Float

eval :: NumExpr -> Float
eval (Binary bOp a b)  = binaryFunction bOp (eval a) (eval b)
eval (Unary uOp e)     = unaryFunction uOp (eval e)
eval (Leaf a)          = a

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
