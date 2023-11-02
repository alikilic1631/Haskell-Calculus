{-# LANGUAGE StandaloneDeriving #-}
module Calculus (lookUp, eval, showExpr, diff, maclaurin) where

import Vars
import Expr

import Data.Maybe

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

-- Comment this out if you want to implement your own instance in terms
-- of `showExpr`
deriving instance Show Expr

instance Num Expr where
  fromInteger = undefined
  negate      = undefined
  (+)         = undefined
  (*)         = undefined

instance Fractional Expr where
  fromRational = undefined
  (/)          = undefined

instance Floating Expr where
  sin = undefined
  cos = undefined
  log = undefined

---------------------------------------------------------------------------

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x w = output
    where output = fromJust (lookup x w)

{-|
Evaluates a given expression, evaluating any variables to their value within
the provided environment.
-}
eval :: Expr -> Env -> Double
eval (Val x) _      = x
eval (Id x) list    = lookUp x list 
eval (Neg x) list   = (eval x list)*(-1)
eval (Mul x y) list = (eval x list)*(eval y list)
eval (Add x y) list = (eval x list)+(eval y list)
eval (Div x y) list = (eval x list)/(eval y list)
eval (Sin x)   list = sin(eval x list)
eval (Cos x)   list = cos(eval x list)
eval (Log x)   list = log(eval x list)


{-| OPTIONAL
Pretty prints an expression to a more human-readable form.
-}
showExpr :: Expr -> String
showExpr = undefined

{-|
Symbolically differentiates a term with respect to a given identifier.
-}
diff :: Expr -> String -> Expr
diff (Val a) _      = Val 0.0
diff (Id a) x 
  | a == x    = Val 1.0
  | otherwise = Val 0.0

diff (Neg a) x   = Neg(diff a x)

diff (Mul a b) x = Add (Mul a (diff b x)) (Mul (diff a x) b) 
diff (Add a b) x = Add (diff a x) (diff b x)
diff (Div a b) x = Div (Add (Mul b (diff a x)) (Neg (Mul a (diff b x)))) (Mul b b)

diff (Sin a)   x = Mul (Cos a) (diff a x)
diff (Cos a)   x = Neg (Mul  (Sin a) (diff a x))
diff (Log a)   x = Div (diff a x) a



{-|
Calculates the factorial of a Double value recursively.
-}
fac :: Int -> Int
fac 0 = 1
fac 1 = 1
fac n   = n*fac (n-1)

{-|
Computes the approximation of an expression `f` by expanding the Maclaurin
series on `f` and taking its summation.
-}

maclaurin :: Expr   -- ^ expression to approximate (with `x` free)
          -> Double -- ^ value to give to `x`
          -> Int    -- ^ number of terms to expand
          -> Double -- ^ the approximate result
maclaurin expr x order = helper expr x order 0
    where 
      helper :: Expr -> Double -> Int-> Int -> Double
      helper expr x order term_num 
        | term_num == order = 0.0
        | term_num == 0     = eval expr val_list + helper expr x order 1
        | otherwise         = (eval f_prime val_list *(x_n / denom)) + helper f_prime x order (term_num+1)
        where 
          val_list = [("x",0.0)]
          f_prime = diff expr "x"
          x_n = x^term_num
          denom = fromIntegral(fac term_num)
