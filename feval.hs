module Feval
( Result(..)
, run
) where

import AST
import EvalAST (evalTransform, RVal)
import TypeAST (typeTransform, FType)
import Algebra
import Type
import Eval

data Result = Result (RVal, FType) | TypeMismatch | InconsistentTypes deriving Show

run :: Fix Expr -> Result
run e = case typecheck (typeTransform e) of
    Nothing -> InconsistentTypes
    Just t -> case eval (evalTransform e) of
        Nothing -> TypeMismatch
        Just v -> Result (v, t)

