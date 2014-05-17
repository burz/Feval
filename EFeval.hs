{-# LANGUAGE FlexibleInstances #-}

module EFeval
( F.Result(..)
, Expr(..)
, run
) where

import Algebra
import qualified AST as AST
import qualified Feval as F

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
    | Not a
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
    fmap eval (Not x) = Not $ eval x
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
    show (Fx (Not x)) = "!" ++ (case x of
        (Fx (CBool b)) -> show b
        (Fx (CVar s)) -> s
        _ -> "(" ++ show x ++ ")")
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

recursiveAlg :: String -> Algebra AST.Expr Bool
recursiveAlg _ (AST.CInt n) = False
recursiveAlg _ (AST.CBool b) = False
recursiveAlg s (AST.CVar s') = if s' == s then True else False
recursiveAlg _ (AST.Add x y) = x || y
recursiveAlg _ (AST.Sub x y) = x || y
recursiveAlg _ (AST.Mul x y) = x || y
recursiveAlg _ (AST.Div x y) = x || y
recursiveAlg _ (AST.And x y) = x || y
recursiveAlg _ (AST.Or x y) = x || y
recursiveAlg _ (AST.Not x) = x
recursiveAlg _ (AST.Equal x y) = x || y
recursiveAlg _ (AST.If p x y) = p || x || y
recursiveAlg s (AST.Function s' p) = if s' == s then False else p
recursiveAlg _ (AST.Appl f x) = f || x
recursiveAlg s (AST.LetRec f x p e) = if f == s
    then False
    else if x == s then p else p || e

recursive :: String -> Fix AST.Expr -> Bool
recursive s = cata $ recursiveAlg s

createWrapper :: [String] -> Fix AST.Expr -> Fix AST.Expr
createWrapper [] e = e
createWrapper (x:xs) e = Fx $ AST.Function x (createWrapper xs e)

letTransform :: String -> Fix AST.Expr -> Fix AST.Expr -> Fix AST.Expr
letTransform s x y = Fx $ AST.Appl (Fx $ AST.Function s y) x

alg :: Algebra Expr (Fix AST.Expr)
alg (CInt n) = Fx $ AST.CInt n
alg (CBool b) = Fx $ AST.CBool b
alg (CVar s) = Fx $ AST.CVar s
alg (Add x y) = Fx $ AST.Add x y
alg (Sub x y) = Fx $ AST.Sub x y
alg (Mul x y) = Fx $ AST.Mul x y
alg (Div x y) = Fx $ AST.Div x y
alg (And x y) = Fx $ AST.And x y
alg (Or x y) = Fx $ AST.Or x y
alg (Not x) = Fx $ AST.Not x
alg (Equal x y) = Fx $ AST.Equal x y
alg (If p x y) = Fx $ AST.If p x y
alg (Function s p) = Fx $ AST.Function s p
alg (Appl f x) = Fx $ AST.Appl f x
alg (Let f [] p e) = letTransform f p e
alg (Let f (a:as) p e) = if recursive f p
    then Fx $ AST.LetRec f a (createWrapper as p) e
    else letTransform f (createWrapper (a:as) p) e
alg (Semi x y) = Fx $ AST.Appl (Fx $ AST.Function "_" y) x

translate :: Fix Expr -> Fix AST.Expr
translate = cata alg

run :: Fix Expr -> F.Result
run = F.run . translate

