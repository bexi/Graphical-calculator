
module Expr where

import Test.QuickCheck

-------------------------------------------------------------------------

-- test data
expr1 = Add (Num 5) (Num 3)
expr2 = Mul expr1 expr1
expr3 = Add expr2 (Num 6)
expr4 = Mul (Num 5) (Num 3)
expr5 = Add expr2 expr4
expr6 = Add expr4 expr4

instance Arbitrary Expr where
  arbitrary = sized arbExpr

-- represents expressions
data Expr =   Num Double
            | Add Expr Expr
            | Mul Expr Expr
            | Sin Expr
            | Cos Expr
            | X

-- converts any expression to string
showExpr :: Expr -> String
showExpr (Num n) | isInt n        = show (round n)
showExpr (Num n) | not (isInt n)  = show n
showExpr (Add a b)                = showExpr a ++ "+" ++ showExpr b
showExpr (Mul a b)                = showFact a ++ "*" ++ showFact b

showFact :: Expr -> String
showFact (Num n)    = showExpr (Num n)
showFact (Add a b)  = "(" ++ showExpr (Add a b) ++ ")"
showFact (Mul a b)  = showExpr (Mul a b)

isInt :: Double -> Bool
isInt x = x == fromInteger (round x)

-- calculates the value of the expression
eval :: Expr -> Double -> Double
eval = undefined

-- tries to interpret the string as an expression,
-- and returns Just of that expression if it succeeds.
-- Otherwise, Nothing will be returned.
readExpr :: String -> Maybe Expr
readExpr = undefined

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
