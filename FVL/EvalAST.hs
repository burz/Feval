{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module FVL.EvalAST
( Expr(..)
, RVal(..)
, valueTransform
, showCons
, evalTransform
) where

import FVL.Algebra
import qualified FVL.FAST as FAST

data Expr a b
    = CInt Integer
    | CBool Bool
    | CVar String
    | Add b b
    | Sub b b
    | Mul b b
    | Div b b
    | And b b
    | Or b b
    | Not b
    | Equal b b
    | Less b b
    | Empty
    | Cons b b
    | If b a a
    | Function String a
    | Appl b a
    | LetRec String String a a
    | Case b a String String a
    deriving Functor

showCons' :: LazyFix Expr -> [LazyFix Expr]
showCons' (Fx' (x `Cons` y)) = x : showCons' y
showCons' e = [e]

showCons :: LazyFix Expr -> String
showCons e = "[" ++ (foldr combine "\b\b" (showCons' e)) ++ "]"
    where combine (Fx' Empty) b = b
          combine a b = show a ++ ", " ++ b

instance Show (LazyFix Expr) where
    show (Fx' (CInt n)) = show n
    show (Fx' (CBool b)) = show b
    show (Fx' (CVar s)) = s
    show (Fx' (x `Add` y)) = show x ++ " + " ++ show y
    show (Fx' (x `Sub` y)) = show x ++ " - " ++ show y
    show (Fx' (x `Mul` y)) = show x ++ " * " ++ show y
    show (Fx' (x `Div` y)) = show x ++ " / " ++ show y
    show (Fx' (x `And` y)) = show x ++ " && " ++ show y
    show (Fx' (x `Or` y)) = show x ++ " || " ++ show y
    show (Fx' (Not x)) = "!" ++ (case x of
        (Fx' (CBool b)) -> show b
        (Fx' (CVar s)) -> s
        _ -> "(" ++ show x ++ ")")
    show (Fx' (x `Equal` y)) = show x ++ " = " ++ show y
    show (Fx' (x `Less` y)) = show x ++ " < " ++ show y
    show (Fx' Empty) = "[]"
    show (Fx' (x `Cons` y)) = showCons . Fx' $ x `Cons` y
    show (Fx' (If p x y)) = "If " ++ show p ++ " Then " ++ show x ++ " Else " ++ show y
    show (Fx' (Function x p)) = "Function " ++ x ++ " -> " ++ show p
    show (Fx' (Appl f x)) = (case f of
        (Fx' (CInt n)) -> show n ++ " "
        (Fx' (CBool b)) -> show b ++ " "
        (Fx' (CVar s)) -> s ++ " "
        (Fx' (Appl _ _)) -> show f ++ " "
        _ -> "(" ++ show f ++ ") ") ++ (case x of
            (Fx' (CInt n)) -> show n
            (Fx' (CBool b)) -> show b
            (Fx' (CVar s)) -> s
            (Fx' (Appl _ _)) -> show x
            _ -> "(" ++ show x ++ ")")
    show (Fx' (LetRec f x p e))
        = "Let Rec " ++ f ++ " " ++ x ++ " = " ++ show p ++ " In " ++ show e
    show (Fx' (Case p x s t y)) = "Case " ++ show x ++ " Of [] -> " ++ show x
        ++ " | (" ++ s ++ ", " ++ t ++ ") -> " ++ show y

data RVal = RInt Integer
          | RBool Bool
          | RFunction String (LazyFix Expr)
          | REmpty
          | RCons RVal RVal

valueTransform :: RVal -> LazyFix Expr
valueTransform (RInt n) = Fx' $ CInt n
valueTransform (RBool b) = Fx' $ CBool b
valueTransform (RFunction s p) = Fx' $ Function s p
valueTransform REmpty = Fx' $ Empty
valueTransform (RCons x y) = Fx' $ Cons (valueTransform x) (valueTransform y)

instance Show RVal where
    show = show . valueTransform

alg :: Algebra FAST.Expr (LazyFix Expr)
alg (FAST.CInt n) = Fx' $ CInt n
alg (FAST.CBool b) = Fx' $ CBool b
alg (FAST.CVar s) = Fx' $ CVar s
alg (FAST.Add x y) = Fx' $ Add x y
alg (FAST.Sub x y) = Fx' $ Sub x y
alg (FAST.Mul x y) = Fx' $ Mul x y
alg (FAST.Div x y) = Fx' $ Div x y
alg (FAST.And x y) = Fx' $ And x y
alg (FAST.Or x y) = Fx' $ Or x y
alg (FAST.Not x) = Fx' $ Not x
alg (FAST.Equal x y) = Fx' $ Equal x y
alg (FAST.Less x y) = Fx' $ Less x y
alg (FAST.Empty) = Fx' $ Empty
alg (FAST.Cons x y) = Fx' $ Cons x y
alg (FAST.If p x y) = Fx' $ If p x y
alg (FAST.Function s p) = Fx' $ Function s p
alg (FAST.Appl f x) = Fx' $ Appl f x
alg (FAST.LetRec f x p e) = Fx' $ LetRec f x p e
alg (FAST.Case p x s t y) = Fx' $ Case p x s t y

evalTransform :: Fix FAST.Expr -> LazyFix Expr
evalTransform = cata alg

