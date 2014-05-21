module FAST
( Expr(..)
) where

import Algebra

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
    fmap eval (x `Less` y) = eval x `Less` eval y
    fmap eval Empty = Empty
    fmap eval (x `Cons` y) = eval x `Cons` eval y
    fmap eval (If p e1 e2) = If (eval p) (eval e1) (eval e2)
    fmap eval (Function s p) = Function s (eval p)
    fmap eval (Appl f x) = Appl (eval f) (eval x)
    fmap eval (LetRec f x p e) = LetRec f x (eval p) (eval e)
    fmap eval (Case p x s t y) = Case (eval p) (eval x) s t (eval y)

