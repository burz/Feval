module AST
( Expr(..)
) where

import Algebra

data Expr a
    = CInt Int
    | CBool Bool
    | CVar String
    | Add a a
    | Mul a a
    | And a a
    | Or a a
    | Equal a a
    | If a a a
    | Function String a
    | Appl a a
    | LetRec String String a a

instance Functor Expr where
    fmap eval (CInt n) = CInt n
    fmap eval (CBool b) = CBool b
    fmap eval (CVar s) = CVar s
    fmap eval (x `Add` y) = eval x `Add` eval y
    fmap eval (x `Mul` y) = eval x `Mul` eval y
    fmap eval (x `And` y) = eval x `And` eval y
    fmap eval (x `Or` y) = eval x `Or` eval y
    fmap eval (x `Equal` y) = eval x `Equal` eval y
    fmap eval (If p e1 e2) = If (eval p) (eval e1) (eval e2)
    fmap eval (Function s p) = Function s (eval p)
    fmap eval (Appl f x) = Appl (eval f) (eval x)
    fmap eval (LetRec f x p e) = LetRec f x (eval p) (eval e)

