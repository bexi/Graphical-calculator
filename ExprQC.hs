module ExprQC where

import Expr
import Test.QuickCheck
import Data.Maybe

instance Arbitrary Expr where
  arbitrary = sized arbExpr

-- property for show and read functions
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr expr = (fromJust (readExpr (showExpr expr))) == expr

-- generator for expressions
arbExpr :: Int -> Gen Expr
arbExpr size = frequency [(1, num), (size, operation), (size, function)]
  where num = elements [Num n | n<-[0..100]]
        var = elements [X]
        operation = do op <- elements [Operator mul, Operator plus]
                       let size' = size `div` 2
                       a <- arbExpr size'
                       b <- arbExpr size'
                       return (op a b)
        function  = do fu <- elements [Function cos', Function sin']
                       let size' = size `div` 2
                       a <- arbExpr size'
                       return (fu a)

-- property for simplify function
prop_simplify :: Expr -> Bool
prop_simplify expr = eval (simplify expr) 1 == eval expr 1
