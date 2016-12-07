
module Test where

import Test.QuickCheck
import Parsing

-------------------------------------------------------------------------

instance Arbitrary Expr where
  arbitrary = sized arbExpr

-- represents expressions
data Expr =   Num Double
            | Operator Op Expr Expr
            | Function Fu Expr
            | X

data Op = Op {func :: Double -> Double -> Double,
              name :: String,
              prio :: Int}

data Fu = Fu {funcF :: Double -> Double,
              nameF :: String,
              prioF :: Int}

--operations
mul  = Op {func=(*), name="*", prio=2}
plus = Op {func=(+), name="+", prio=1}
--functions
cos' = Fu {funcF=(cos), nameF="cos ", prioF=3}
sin' = Fu {funcF=(sin), nameF="sin ", prioF=3}
--test data
expr1 = Operator plus (Num 2) (Num 8) --2+8
expr2 = Operator mul (Num 1) (Num 7)  --1*7
expr3 = Operator plus expr1 expr2     --2+8+1*7
expr4 = Operator mul expr1 expr2      --(2+8)*1*7
expr5 = Function cos' (Num 5)         --cos 5
expr6 = Function sin' expr3           --sin (2+8+1*7)
expr7 = Function sin' (Function sin' (Num 5)) -- cos sin 5

-- converts any expression to string
showExpr :: Expr -> String
showExpr (X)                      = "x"
showExpr (Num n) | isInt n        = show (round n)
showExpr (Num n) | not (isInt n)  = show n
showExpr (Operator o e1 e2) | (prio o == 1) = showExpr e1 ++ name o ++ showExpr e2
showExpr (Operator o e1 e2) | (prio o == 2) = showExprOp e1 ++ name o ++ showExprOp e2
showExpr (Function o e)                     = nameF o ++ showExprFu e

--only called if parent op had prio 2 (mul)
showExprOp (Operator o e1 e2) | (prio o == 1) = "(" ++ showExpr e1 ++ name o ++ showExpr e2 ++ ")"
showExprOp (Operator o e1 e2) | (prio o == 2) = showExprOp e1 ++ name o ++ showExprOp e2
showExprOp (Function o e)                     = nameF o ++ showExprFu e
showExprOp (X)                                = "x"
showExprOp (Num n)                            = showExpr (Num n)

--only called if parent was function
showExprFu (Operator o e1 e2) | (prio o == 1) = "(" ++ showExpr e1 ++ name o ++ showExpr e2 ++ ")"
showExprFu (Operator o e1 e2) | (prio o == 2) = "(" ++ showExprOp e1 ++ name o ++ showExprOp e2 ++ ")"
showExprFu (Function o e)     = nameF o ++ showExprFu e
showExprFu (X)                = "x"
showExprFu (Num n)            = showExpr (Num n)


{-showExpr (Operator o e1 e2) | (prio o == 1) = showExpr e1 ++ name o ++ showExpr e2
showExpr (Operator o e1 e2) | (prio o == 2) = showExpr' e1 ++ name o ++ showExpr' e2
showExpr (Function o e)                     = nameF o ++ "(" ++ showExpr e ++ ")" -}

{-showExpr' :: Expr -> String
showExpr' (Num n) = showExpr (Num n)
showExpr' (Operator o e1 e2) | (prio o == 1) = "(" ++ showExpr e1 ++ name o ++ showExpr e2 ++ ")"
showExpr' (Operator o e1 e2) | (prio o == 2) = "(" ++ showExpr' e1 ++ name o ++ showExpr' e2 ++ ")"
showExpr' (Function o e)                     = showExpr (Function o e) -}

isInt :: Double -> Bool
isInt x = x == fromInteger (round x)

-- calculates the value of the expression,
--the second parameter is the wanted value of X
eval :: Expr -> Double -> Double
eval X x                  = x
eval (Num n) x            = n
eval (Operator o e1 e2) x = (func o) (eval e1 x) (eval e2 x)
eval (Function o e) x     = (funcF o) (eval e x)

-- tries to interpret the string as an expression,
-- and returns Just of that expression if it succeeds.
-- Otherwise, Nothing will be returned.
readExpr :: String -> Maybe Expr
readExpr s = undefined

-- property for show and read functions
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr = undefined

-- generator for expressions
arbExpr :: Int -> Gen Expr
arbExpr = undefined

-- simplyfy an expression to its smallest representation
simplify :: Expr -> Expr
simplify = undefined

-- differentiates the expression (with respect to x)
differentiate :: Expr -> Expr
differentiate = undefined
