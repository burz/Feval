{-# LANGUAGE FlexibleInstances #-}

module FVL.F
( run_eval
, run_typecheck
, Result(..)
, run
) where

import FVL.FAST
import FVL.EvalAST (evalTransform, RVal)
import FVL.TypeAST (typeTransform, FType)
import FVL.Algebra
import FVL.Type
import FVL.Eval

instance Show (Fix Expr) where
    show = show . evalTransform

run_eval :: Fix Expr -> Maybe RVal
run_eval = eval . evalTransform

run_typecheck :: Fix Expr -> Maybe FType
run_typecheck = typecheck . typeTransform

data Result = Result (RVal, FType) | TypeMismatch | InconsistentTypes deriving Show

run :: Fix Expr -> Result
run e = case run_typecheck e of
    Nothing -> InconsistentTypes
    Just t -> case run_eval e of
        Nothing -> TypeMismatch
        Just v -> Result (v, t)

