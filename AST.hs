module AST
( CVal(..)
, ExprF(..)
) where

data CVal = CInt Int | CBool Bool | MismatchedTypes deriving Show

data ExprF a
    = Const CVal
    | Add a a
    | Mul a a
    | And a a
    | Or a a
    | Equal a a
    | If a a a

instance Functor ExprF where
    fmap eval (Const i) = Const i
    fmap eval (x `Add` y) = (eval x) `Add` (eval y)
    fmap eval (x `Mul` y) = (eval x) `Mul` (eval y)
    fmap eval (x `And` y) = (eval x) `And` (eval y)
    fmap eval (x `Or` y) = (eval x) `Or` (eval y)
    fmap eval (x `Equal` y) = (eval x) `Equal` (eval y)
    fmap eval (If p e1 e2) = If (eval p) (eval e1) (eval e2)

