{-# LANGUAGE FlexibleInstances #-}

module EvalAST
( Expr(..)
, RVal(..)
, evalTransform
) where

import Algebra
import qualified AST as AST

data Expr a b
    = CInt Int
    | CBool Bool
    | CVar String
    | Add b b
    | Sub b b
    | Mul b b
    | Div b b
    | And b b
    | Or b b
    | Equal b b
    | If b a a
    | Function String a
    | Appl b a
    | LetRec String String a a

instance Functor (Expr (LazyFix Expr)) where
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
    fmap eval (If p e1 e2) = If (eval p) e1 e2
    fmap eval (Function s p) = Function s p
    fmap eval (Appl f x) = Appl (eval f) x
    fmap eval (LetRec f x p e) = LetRec f x p e

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
    show (Fx' (x `Equal` y)) = show x ++ " = " ++ show y
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

data RVal = RInt Int | RBool Bool | RFunction String (LazyFix Expr)

instance Show RVal where
    show (RInt n) = show n
    show (RBool b) = show b
    show (RFunction x e) = "Function " ++ x ++ " -> " ++ show e

alg :: Algebra AST.Expr (LazyFix Expr)
alg (AST.CInt n) = Fx' $ CInt n
alg (AST.CBool b) = Fx' $ CBool b
alg (AST.CVar s) = Fx' $ CVar s
alg (AST.Add x y) = Fx' $ Add x y
alg (AST.Sub x y) = Fx' $ Sub x y
alg (AST.Mul x y) = Fx' $ Mul x y
alg (AST.Div x y) = Fx' $ Div x y
alg (AST.And x y) = Fx' $ And x y
alg (AST.Or x y) = Fx' $ Or x y
alg (AST.Equal x y) = Fx' $ Equal x y
alg (AST.If p x y) = Fx' $ If p x y
alg (AST.Function s p) = Fx' $ Function s p
alg (AST.Appl f x) = Fx' $ Appl f x
alg (AST.LetRec f x p e) = Fx' $ LetRec f x p e

evalTransform :: Fix AST.Expr -> LazyFix Expr
evalTransform = cata alg

