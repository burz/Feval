module Eval
( eval
) where

import Control.Applicative

import EvalAST hiding (transform)
import Algebra

type EvalAlgebra = Algebra (Expr (LazyFix Expr)) (Maybe RVal)

integer_operation :: (Int -> Int -> Int) -> RVal -> RVal -> Maybe RVal
integer_operation f (RInt x) (RInt y) = Just . RInt $ f x y
integer_operation _ _ _ = Nothing

boolean_operation :: (Bool -> Bool -> Bool) -> RVal -> RVal -> Maybe RVal
boolean_operation f (RBool x) (RBool y) = Just . RBool $ f x y
boolean_operation _ _ _ = Nothing

transform :: RVal -> LazyFix Expr
transform (RInt n) = Fx' $ CInt n
transform (RBool b) = Fx' $ CBool b
transform (RFunction s p) = Fx' $ Function s p

substitute :: String -> RVal -> LazyFix Expr -> LazyFix Expr
substitute _ _ (Fx' (CInt n)) = Fx' $ CInt n
substitute _ _ (Fx' (CBool b)) = Fx' $ CBool b
substitute s v (Fx' (CVar s')) = if s' == s then transform v else Fx' $ CVar s'
substitute s v (Fx' (Add x y)) = Fx' $ Add (substitute s v x) (substitute s v y)
substitute s v (Fx' (Sub x y)) = Fx' $ Sub (substitute s v x) (substitute s v y)
substitute s v (Fx' (Mul x y)) = Fx' $ Mul (substitute s v x) (substitute s v y)
substitute s v (Fx' (Div x y)) = Fx' $ Div (substitute s v x) (substitute s v y)
substitute s v (Fx' (And x y)) = Fx' $ And (substitute s v x) (substitute s v y)
substitute s v (Fx' (Or x y)) = Fx' $ Or (substitute s v x) (substitute s v y)
substitute s v (Fx' (Not x)) = Fx' . Not $ substitute s v x
substitute s v (Fx' (Equal x y)) = Fx' $ Equal (substitute s v x) (substitute s v y)
substitute s v (Fx' (If p x y))
    = Fx' $ If (substitute s v p) (substitute s v x) (substitute s v y)
substitute s v (Fx' (Function x p)) = Fx' $ if x == s
    then Function x p
    else Function x (substitute s v p)
substitute s v (Fx' (Appl f x)) = Fx' $ Appl (substitute s v f) (substitute s v x)
substitute s v (Fx' (LetRec f x p e)) = Fx' $ if f == s
    then LetRec f x p e
    else if x == s
        then LetRec f x p (substitute s v e)
        else LetRec f x (substitute s v p) (substitute s v e)

apply :: RVal -> LazyFix Expr -> Maybe RVal
apply (RFunction x p) e = eval e >>= \v -> eval $ substitute x v p
apply _ _ = Nothing

alg :: EvalAlgebra
alg (CInt n) = Just $ RInt n
alg (CBool b) = Just $ RBool b
alg (CVar s) = Nothing
alg (x `Add` y) = x >>= \x' -> y >>= \y' -> integer_operation (+) x' y'
alg (x `Sub` y) = x >>= \x' -> y >>= \y' -> integer_operation (-) x' y'
alg (x `Mul` y) = x >>= \x' -> y >>= \y' -> integer_operation (*) x' y'
alg (x `Div` y) = x >>= \x' -> y >>= \y' -> integer_operation quot x' y'
alg (x `And` y) = x >>= \x' -> y >>= \y' -> boolean_operation (&&) x' y'
alg (x `Or` y) = x >>= \x' -> y >>= \y' -> boolean_operation (||) x' y'
alg (Not x) = x >>= \x' -> case x' of
    RBool b -> Just . RBool $ not b
    _ -> Nothing
alg (x `Equal` y) = x >>= \x' -> y >>= \y' -> case (x', y') of
    (RInt m, RInt n) -> Just . RBool $ m == n
    _ -> Nothing
alg (If p e1 e2) = p >>= \p' -> case p' of
    RBool r -> if r then eval e1 else eval e2
    _ -> Nothing
alg (Function x p) = Just $ RFunction x p
alg (Appl f x) = f >>= \f' -> apply f' x
alg (LetRec f x p e) =
    let e' = Fx' $ LetRec f x p (Fx' $ Appl (Fx' $ CVar f) (Fx' $ CVar x)) in
    let r = RFunction x (Fx' $ LetRec f x p e') in
    let r' = substitute f r p in eval $ substitute f (RFunction x r') e

eval :: LazyFix Expr -> Maybe RVal
eval = lazyCata alg

