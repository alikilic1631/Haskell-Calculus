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
  fromInteger n = Val (fromInteger n)
  negate = Neg 
  (*)    = Mul
  (+)    = Add

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
eval (Sin x)   list = sin (eval x list)
eval (Cos x)   list = cos (eval x list)
eval (Log x)   list = log (eval x list)


{-| OPTIONAL
Pretty prints an expression to a more human-readable form.
-}
showExpr :: Expr -> String
showExpr (Val a)   = show a
showExpr (Id a)    = a
showExpr (Add a b) = "(" ++ showExpr a ++ "+" ++ showExpr b ++ ")"
showExpr (Div a b) = "(" ++ showExpr a ++ "/" ++ showExpr b ++ ")"
showExpr (Mul a b) = "(" ++ showExpr a ++ "*" ++ showExpr b ++ ")"
showExpr (Neg a)   = "-(" ++ showExpr a ++ ")"
showExpr (Sin a)   = "sin(" ++ showExpr a ++ ")"
showExpr (Cos a)   = "cos(" ++ showExpr a ++ ")"
showExpr (Log a)   = "log(" ++ showExpr a ++ ")"


{-|
Symbolically differentiates a term with respect to a given identifier.
-}
diff :: Expr -> String -> Expr
diff (Val a) _   = Val 0.0
diff (Id a) x
  | a == x       = Val 1.0
  | otherwise    = Val 0.0

diff (Neg a) x   = Neg (diff a x)

diff (Mul a b) x = Add (Mul a (diff b x)) (Mul (diff a x) b)
diff (Add a b) x = Add (diff a x) (diff b x)
diff (Div a b) x = Div (Add (Mul b (diff a x)) (Neg (Mul a (diff b x)))) (Mul b b)

diff (Sin a)   x = Mul (Cos a) (diff a x)
diff (Cos a)   x = Neg (Mul  (Sin a) (diff a x))
diff (Log a)   x = Div (diff a x) a



{-|
Calculates the factorial of a integer value recursively.
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
maclaurin expr x order = sum ( zipWith (*) eval_list (zipWith (/) coeff_list factorials) )
  where
    diff_list = take order $ iterate (`diff` "x") expr
    eval_list = map (\exp -> eval exp [("x",0.0)]) diff_list
    coeff_list = take order $ iterate (* x) 1
    factorials = take order $ scanl (*) 1 [1..]


{-
Maclaurin function using a helper function
I would be very happy if you could also grade this version of maclaurin.
By the way this passed all tests.

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
-}
