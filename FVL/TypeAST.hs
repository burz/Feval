{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module FVL.TypeAST
( Expr(..)
, FType(..)
, typeTransform
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
    | If b b b
    | Function String a
    | Appl b b
    | LetRec String String a a
    | Case b b String String a
    deriving Functor

data FType = FInt
           | FBool
           | FVar Int
           | FArrow FType FType
           | FList FType
           | FNotClosed deriving (Eq, Ord)

instance Show FType where
    show FInt = "Int"
    show FBool = "Bool"
    show (FVar n) = "'a" ++ show n
    show (FArrow x y) = show x ++ " -> " ++ show y
    show (FList t) = "[" ++ show t ++ "]"
    show _ = ""

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
alg FAST.Empty = Fx' $ Empty
alg (FAST.Cons x y) = Fx' $ Cons x y
alg (FAST.If p x y) = Fx' $ If p x y
alg (FAST.Function s p) = Fx' $ Function s p
alg (FAST.Appl f x) = Fx' $ Appl f x
alg (FAST.LetRec f x p e) = Fx' $ LetRec f x p e
alg (FAST.Case p x s t y) = Fx' $ Case p x s t y

typeTransform :: Fix FAST.Expr -> LazyFix Expr
typeTransform = cata alg

