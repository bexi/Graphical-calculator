
module Expr where

import Test.QuickCheck

-------------------------------------------------------------------------

-- test data for Add and Mul
expr1 = Add (Num 5) (Num 3)  -- 8
expr2 = Mul expr1 expr1     -- 64
expr3 = Add expr2 (Num 6)   -- 70
expr4 = Mul (Num 5) (Num 3) -- 15
expr5 = Add expr2 expr4     -- 79
expr6 = Add expr4 expr4     -- 30
-- test data for Sin and Cos
expr7 = Sin (Num 5)
expr8 = Sin expr1
expr9 = Cos expr6
expr10 = Cos (Add X expr1)  -- x=5 --> 0.9 

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
showExpr (X)                      = "x"
showExpr (Num n) | isInt n        = show (round n)
showExpr (Num n) | not (isInt n)  = show n
showExpr (Add a b)                = showExpr a ++ "+" ++ showExpr b
showExpr (Mul a b)                = showFact a ++ "*" ++ showFact b
showExpr (Sin a)                  = "sin " ++ showTrig a
showExpr (Cos a)                  = "cos " ++ showTrig a

showFact :: Expr -> String
showFact (Num n)    = showExpr (Num n)
showFact (Add a b)  = "(" ++ showExpr (Add a b) ++ ")"
showFact (Mul a b)  = showExpr (Mul a b)

showTrig :: Expr -> String
showTrig (X)        = showExpr (X)
showTrig (Num n)    = showExpr (Num n)
showTrig (Add a b)  = "(" ++ showExpr (Add a b) ++ ")"
showTrig (Mul a b)  = "(" ++ showExpr (Mul a b) ++ ")"


isInt :: Double -> Bool
isInt x = x == fromInteger (round x)

-- calculates the value of the expression,
--the second parameter is the wanted value of X
eval :: Expr -> Double -> Double
eval (X)       x = x
eval (Num n)   x = n
eval (Add a b) x = (eval a x) + (eval b x)
eval (Mul a b) x = (eval a x) * (eval b x)
eval (Sin a)   x = sin (eval a x)
eval (Cos a)   x = cos (eval a x)

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
