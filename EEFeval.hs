{-# LANGUAGE FlexibleInstances #-}

module EEFeval
( EF.Result(..)
, Expr(..)
, run
) where

import Algebra
import qualified EFeval as EF

data Expr a
    = CInt Int
    | CBool Bool
    | CVar String
    | Add a a
    | Sub a a
    | Mul a a
    | Div a a
    | And a a
    | Or a a
    | Equal a a
    | If a a a
    | Function String a
    | Appl a a
    | Let String [String] a a
    | Semi a a

instance Functor Expr where
    fmap eval (CInt n) = CInt n
    fmap eval (CBool b) = CBool b
    fmap eval (CVar s) = CVar s
    fmap eval (x `Add` y) = eval x `Add` eval y
    fmap eval (x `Sub` y) = eval x `Sub` eval y
    fmap eval (x `Mul` y) = eval x `Mul` eval y
    fmap eval (x `Div` y) = eval x `Div` eval y
    fmap eval (x `And` y) = eval x `And` eval y
    fmap eval (x `Or` y) = eval x `Or` eval y
    fmap eval (x `Equal` y) = eval x `Equal` eval y
    fmap eval (If p e1 e2) = If (eval p) (eval e1) (eval e2) 
    fmap eval (Function s p) = Function s (eval p)
    fmap eval (Appl f x) = Appl (eval f) (eval x)
    fmap eval (Let s a x y) = Let s a (eval x) (eval y)
    fmap eval (Semi x y) = Semi (eval x) (eval y)

instance Show (Fix Expr) where
    show (Fx (CInt n)) = show n
    show (Fx (CBool b)) = show b
    show (Fx (CVar s)) = s
    show (Fx (x `Add` y)) = show x ++ " + " ++ show y
    show (Fx (x `Sub` y)) = show x ++ " - " ++ show y
    show (Fx (x `Mul` y)) = show x ++ " * " ++ show y
    show (Fx (x `Div` y)) = show x ++ " / " ++ show y
    show (Fx (x `And` y)) = show x ++ " && " ++ show y
    show (Fx (x `Or` y)) = show x ++ " || " ++ show y
    show (Fx (x `Equal` y)) = show x ++ " = " ++ show y
    show (Fx (If p x y)) = "If " ++ show p ++ " Then " ++ show x ++ " Else " ++ show y
    show (Fx (Function x p)) = "Function " ++ x ++ " -> " ++ show p
    show (Fx (Appl f x)) = (case f of
        (Fx (CInt n)) -> show n ++ " "
        (Fx (CBool b)) -> show b ++ " "
        (Fx (CVar s)) -> s ++ " "
        (Fx (Appl _ _)) -> show f ++ " "
        _ -> "(" ++ show f ++ ") ") ++ (case x of
            (Fx (CInt n)) -> show n
            (Fx (CBool b)) -> show b
            (Fx (CVar s)) -> s
            (Fx (Appl _ _)) -> show x
            _ -> "(" ++ show x ++ ")")
    show (Fx (Let f a p e))
        = "Let " ++ f ++ show_args ++ " = " ++ show p ++ " In " ++ show e
        where show_args = foldr (\x s -> " " ++ x ++ s) "" a

argument :: String -> [String] -> Bool
argument s [] = False
argument s (x:xs) = if x == s then True else argument s xs

recursiveAlg :: String -> Algebra EF.Expr Bool
recursiveAlg _ (EF.CInt n) = False
recursiveAlg _ (EF.CBool b) = False
recursiveAlg s (EF.CVar s') = if s' == s then True else False
recursiveAlg _ (EF.Add x y) = x || y
recursiveAlg _ (EF.Sub x y) = x || y
recursiveAlg _ (EF.Mul x y) = x || y
recursiveAlg _ (EF.Div x y) = x || y
recursiveAlg _ (EF.And x y) = x || y
recursiveAlg _ (EF.Or x y) = x || y
recursiveAlg _ (EF.Equal x y) = x || y
recursiveAlg _ (EF.If p x y) = p || x || y
recursiveAlg s (EF.Function s' p) = if s' == s then False else p
recursiveAlg _ (EF.Appl f x) = f || x
recursiveAlg s (EF.Let s' x e) = if s' == s then x else x || e
recursiveAlg s (EF.LetRec f x p e) = if f == s
    then False
    else if x == s then p else p || e
recursiveAlg _ (EF.Semi x y) = x || y

recursive :: String -> Fix EF.Expr -> Bool
recursive s = cata $ recursiveAlg s

createWrapper :: [String] -> Fix EF.Expr -> Fix EF.Expr
createWrapper [] e = e
createWrapper (x:xs) e = Fx $ EF.Function x (createWrapper xs e)

alg :: Algebra Expr (Fix EF.Expr)
alg (CInt n) = Fx $ EF.CInt n
alg (CBool b) = Fx $ EF.CBool b
alg (CVar s) = Fx $ EF.CVar s
alg (Add x y) = Fx $ EF.Add x y
alg (Sub x y) = Fx $ EF.Sub x y
alg (Mul x y) = Fx $ EF.Mul x y
alg (Div x y) = Fx $ EF.Div x y
alg (And x y) = Fx $ EF.And x y
alg (Or x y) = Fx $ EF.Or x y
alg (Equal x y) = Fx $ EF.Equal x y
alg (If p x y) = Fx $ EF.If p x y
alg (Function s p) = Fx $ EF.Function s p
alg (Appl f x) = Fx $ EF.Appl f x
alg (Let f [] p e) = Fx $ EF.Let f p e
alg (Let f (a:as) p e) = if recursive f p
    then Fx $ EF.LetRec f a (createWrapper as p) e
    else Fx $ EF.Let f (createWrapper (a:as) p) e
alg (Semi x y) = Fx $ EF.Semi x y

translate :: Fix Expr -> Fix EF.Expr
translate = cata alg

run :: Fix Expr -> EF.Result
run = EF.run . translate

