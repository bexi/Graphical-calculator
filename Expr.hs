
module Expr where

import Test.QuickCheck

------------------------------------------------------------------------- 

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
showExpr = undefined

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
