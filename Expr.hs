
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
              nameF :: String}

--operations
mul  = Op {func=(*), name="*", prio=2}
plus = Op {func=(+), name="+", prio=1}
--functions
cos' = Fu {funcF=(cos), nameF="cos "}
sin' = Fu {funcF=(sin), nameF="sin "}

-- converts any expression to string
showExpr :: Expr -> String
showExpr (X)                      = "x"
showExpr (Num n) | isInt n        = show (round n)
showExpr (Num n) | not (isInt n)  = show n
showExpr (Operator o e1 e2) | (prio o < prio mul)
  = showExpr e1 ++ name o ++ showExprOp e2
showExpr (Operator o e1 e2) | otherwise
  = showExprOp e1 ++ name o ++ showExprFu e2 -- changed from Op
showExpr (Function o e)
  = nameF o ++ showExprFu e

--showExpr'
showExprOp (Operator o e1 e2) | (prio o < prio mul)
  = "(" ++ showExpr (Operator o e1 e2) ++ ")"
showExprOp e = showExpr e

--showExpr''
showExprFu (Operator o e1 e2) = "(" ++ showExpr (Operator o e1 e2) ++ ")"
showExprFu e = showExpr e

-- checks if the double can be an int
isInt :: Double -> Bool
isInt x = x == fromInteger (round x)

-- calculates the value of the expression,
--the second parameter is the wanted value of X
eval :: Expr -> Double -> Double
eval X x                  = x
eval (Num n) _            = n
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
simplify (Operator o (Num 0) e) | (o == plus) = simplify e
simplify (Operator o e (Num 0)) | (o == plus) = simplify e
simplify (Operator o (Num 0) e) | (o == mul) = (Num 0)
simplify (Operator o e (Num 0)) | (o == mul) = (Num 0)
simplify (Operator o (Num 1) e) | (o == mul) = simplify e
simplify (Operator o e (Num 1)) | (o == mul) = simplify e

simplify (Operator o (Num n1) (Num n2)) = Num (func o n1 n2)
simplify (Operator o e1 e2) | hasVariable (Operator o e1 e2)
  = (Operator o (simplify e1) (simplify e2))
                            | otherwise
  = simplify (Operator o (simplify e1) (simplify e2))

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
differentiate' (Operator o e1 e2) | (o == plus)
  = Operator o (differentiate' e1) (differentiate' e2)
differentiate' (Operator o e1 e2) | (o == mul) -- product rule
  = Operator plus (Operator mul (differentiate' e1) e2)
    (Operator mul (differentiate' e2) e1)
differentiate' (Function f e) | (nameF f == "cos ")
  = Operator mul (differentiate' e) (Operator mul (Num (-1)) (Function sin' e))
differentiate' (Function f e) | (nameF f == "sin ")
  = Operator mul (differentiate' e) (Function cos' e)
