{-# LANGUAGE FlexibleInstances #-}

module TypeAST
( Expr(..)
, FType(..)
, typeTransform
) where

import Algebra
import qualified AST as AST

data Expr a b
    = CInt Int
    | CBool Bool
    | CVar String
    | Add b b
    | Mul b b
    | And b b
    | Or b b
    | Equal b b
    | If b b b
    | Function String a
    | Appl b b

instance Functor (Expr (LazyFix Expr)) where
    fmap eval (CInt n) = CInt n
    fmap eval (CBool b) = CBool b
    fmap eval (CVar s) = CVar s
    fmap eval (x `Add` y) = eval x `Add` eval y
    fmap eval (x `Mul` y) = eval x `Mul` eval y
    fmap eval (x `And` y) = eval x `And` eval y
    fmap eval (x `Or` y) = eval x `Or` eval y
    fmap eval (x `Equal` y) = eval x `Equal` eval y
    fmap eval (If p e1 e2) = If (eval p) (eval e1) (eval e2)
    fmap eval (Function s p) = Function s p
    fmap eval (Appl f x) = Appl (eval f) (eval x)

data FType = FInt | FBool | FVar Int | FArrow FType FType | FNotClosed deriving (Eq, Ord)

instance Show FType where
    show FInt = "Int"
    show FBool = "Bool"
    show (FArrow x y) = show x ++ " -> " ++ show y
    show _ = ""

alg :: Algebra AST.Expr (LazyFix Expr)
alg (AST.CInt n) = Fx' $ CInt n
alg (AST.CBool b) = Fx' $ CBool b
alg (AST.CVar s) = Fx' $ CVar s
alg (AST.Add x y) = Fx' $ Add x y
alg (AST.Mul x y) = Fx' $ Mul x y
alg (AST.And x y) = Fx' $ And x y
alg (AST.Or x y) = Fx' $ Or x y
alg (AST.Equal x y) = Fx' $ Equal x y
alg (AST.If p x y) = Fx' $ If p x y
alg (AST.Function s p) = Fx' $ Function s p
alg (AST.Appl f x) = Fx' $ Appl f x

typeTransform :: Fix AST.Expr -> LazyFix Expr
typeTransform = cata alg

