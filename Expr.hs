
module Expr where

import Parsing

-------------------------------------------------------------------------

instance Show Expr where
  show = showExpr

instance Eq Op where
  (==) o1 o2 = name o1 == name o2

instance Eq Fu where
  (==) o1 o2 = nameF o1 == nameF o2

-- represents expressions
data Expr =   Num Double
            | Operator Op Expr Expr
            | Function Fu Expr
            | X
            deriving (Eq)

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
expr7 = Function sin' (Function sin' (Operator plus (Num 5) (Num 5))) -- cos sin 5
expr8 = Operator plus (Operator plus (Num 6) (Num 3)) (Num 8)
expr9 = Operator plus (Num 6) (Operator plus (Num 3) (Num 8))
addX3 = Operator plus (Num 6) (Operator plus (Operator plus (Num 7) (Num 4)) (Num 1))
mulX3 = Operator mul (Num 6) (Operator mul (Num 1) (Operator mul (Num 7) (Num 4)))

expr10 = Operator mul (Operator mul (Num 6) (Num 3)) (Num 8)
expr11 = Operator mul (Num 6) (Operator mul (Num 3) (Num 8))

expr12 = Function sin' (Function cos' (Operator plus (Num 5) (Function cos' (Num 5)))) -- cos sin 5
expr13 = Operator mul (X) (Operator mul (Num 1) (Operator mul (Num 7) (Num 4)))
expr14 = Operator mul (Num 5) (Operator mul (X) (Operator mul (Num 7) (X)))


expr1S = "2+8"
expr2S = "cos 3"
expr3S = "sin (6+4)"
expr4S = "sin (7*(5+5))"
expr5S = "cos (x*7)"
expr6S = "cos sin x"
expr7S = "1+2+3"

expr1SF = "cos (1+(5+1))" -- ska den gå igenom?
expr2SF = "5+(7*(5+1))"   -- ska den gå igenom?
expr3SF = "(cos 5)"       -- ska gen gå igenom?

-- converts any expression to string
showExpr :: Expr -> String
showExpr (X)                      = "x"
showExpr (Num n) | isInt n        = show (round n)
showExpr (Num n) | not (isInt n)  = show n
showExpr (Operator o e1 e2) | (prio o == 1) = showExpr e1 ++ name o ++ showExprOp e2
showExpr (Operator o e1 e2) | (prio o == 2) = showExprOp e1 ++ name o ++ showExprFu e2 -- changed from Op
showExpr (Function o e)                     = nameF o ++ showExprFu e

--showFactor
showExprOp (Operator o e1 e2) | (prio o == 1) = "(" ++ showExpr (Operator o e1 e2) ++ ")"
showExprOp e = showExpr e

--showFuncParam
showExprFu (Operator o e1 e2) = "(" ++ showExpr (Operator o e1 e2) ++ ")"
showExprFu e = showExpr e


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
readExpr s = getElem (parse expr s)
  where
    getElem (Just (s,_)) = Just s
    getElem Nothing      = Nothing

--help functions for readExpr
expr, term, factor :: Parser Expr
expr = leftAssoc (Operator plus) term (char '+')
term = leftAssoc (Operator mul) factor (char '*')
factor = (Num <$> readsP) <|> (char '(' *> expr <* char ')')
                          <|> do f <- func'
                                 e <- factor
                                 return (Function f e)
                          <|> (do x <- char 'x'
                                  return X)
-- parser for a Fu (function data type)
func' :: Parser Fu
func' = do string "sin "
           return sin'
      <|>
        do string "cos "
           return cos'

-- parser for a string
string :: String -> Parser String
string ""    = return ""
string (c:s) = do c' <- char c
                  s' <- string s
                  return (c':s')

-- input: constructor, parser for result, parser for seperators
leftAssoc :: (t->t->t) -> Parser t -> Parser sep -> Parser t
leftAssoc op item sep = do i:is <- chain item sep
                           return (foldl op i is)

-- simplyfy an expression to its smallest representation
simplify :: Expr -> Expr
-- cases for operations
simplify (Operator o (Num 0) e) | (name o == "+") = simplify e
simplify (Operator o e (Num 0)) | (name o == "+") = simplify e
simplify (Operator o (Num 0) e) | (name o == "*") = (Num 0)
simplify (Operator o e (Num 0)) | (name o == "*") = (Num 0)
simplify (Operator o (Num 1) e) | (name o == "*") = simplify e
simplify (Operator o e (Num 1)) | (name o == "*") = simplify e

simplify (Operator o (Num n1) (Num n2)) = Num (func o n1 n2)
simplify (Operator o e1 e2) | hasVariable (Operator o e1 e2) = (Operator o (simplify e1) (simplify e2))
                            | otherwise = simplify (Operator o (simplify e1) (simplify e2))

simplify (Function f (Num a)) = Num ((funcF f) a)
simplify (Function f e) | hasVariable e = Function f (simplify e)
                        | otherwise = simplify (Function f (simplify e))
simplify e = e

-- checks if an expression contains a variable
hasVariable :: Expr -> Bool
hasVariable X       = True
hasVariable (Num n) = False
hasVariable (Operator o e1 e2) = hasVariable e1 || hasVariable e2
hasVariable (Function f e)     = hasVariable e

-- differentiates the expression (with respect to x)
differentiate :: Expr -> Expr
differentiate expr = simplify (differentiate' expr)

differentiate' :: Expr -> Expr
differentiate' (Num n)             = Num 0
differentiate' (X)                 = Num 1
differentiate' (Operator o e1 e2) | (not (hasVariable (Operator o e1 e2))) = Num 0
differentiate' (Operator o e1 e2) | (name o == "+") = Operator o (differentiate' e1) (differentiate' e2)

differentiate' (Operator o (Num n1) (Num n2))          = Num 0
differentiate' (Operator o (Num n) e) | (hasVariable e) = Operator o (Num n) (differentiate' e)
differentiate' (Operator o e (Num n)) | (hasVariable e) = Operator o (differentiate' e) (Num n)

differentiate' (Function f e) | not (hasVariable (Function f e)) = (Num 0)
differentiate' (Function f e) | (nameF f == "cos ") = Operator mul (differentiate' e) (Operator mul (Num (-1)) (Function sin' e))
differentiate' (Function f e) | (nameF f == "sin ") = Operator mul (differentiate' e) (Function cos' e)
