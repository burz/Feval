module Eval
( RVal(..)
, eval
) where

import Control.Applicative

import EvalAST
import Algebra

data RVal = RInt Int | RBool Bool deriving Show

type EvalAlgebra = Algebra (Expr (LazyFix Expr)) (Maybe RVal)

integer_operation :: (Int -> Int -> Int) -> RVal -> RVal -> Maybe RVal
integer_operation f (RInt x) (RInt y) = Just . RInt $ f x y
integer_operation _ _ _ = Nothing

boolean_operation :: (Bool -> Bool -> Bool) -> RVal -> RVal -> Maybe RVal
boolean_operation f (RBool x) (RBool y) = Just . RBool $ f x y
boolean_operation _ _ _ = Nothing

alg :: EvalAlgebra
alg (CInt n) = Just $ RInt n
alg (CBool b) = Just $ RBool b
alg (CVar s) = Nothing
alg (x `Add` y) = x >>= \x' -> y >>= \y' -> integer_operation (+) x' y'
alg (x `Mul` y) = x >>= \x' -> y >>= \y' -> integer_operation (*) x' y'
alg (x `And` y) = x >>= \x' -> y >>= \y' -> boolean_operation (&&) x' y'
alg (x `Or` y) = x >>= \x' -> y >>= \y' -> boolean_operation (||) x' y'
alg (x `Equal` y) = x >>= \x' -> y >>= \y' -> case (x', y') of
    (RInt m, RInt n) -> Just . RBool $ m == n
    _ -> Nothing
alg (If p e1 e2) = p >>= \p' -> case p' of
    RBool r -> if r then eval e1 else eval e2
    _ -> Nothing

eval :: LazyFix Expr -> Maybe RVal
eval = lazyCata alg

