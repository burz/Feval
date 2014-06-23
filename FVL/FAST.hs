{-# LANGUAGE DeriveFunctor #-}

module FVL.FAST
( Expr(..)
) where

import FVL.Algebra

data Expr a
    = CInt Integer
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
    | Less a a
    | Empty
    | Cons a a
    | If a a a
    | Function String a
    | Appl a a
    | LetRec String String a a
    | Case a a String String a
    deriving Functor

