import AST
import EvalAST (evalTransform, RVal)
import TypeAST (typeTransform, FType)
import Algebra
import Type
import Eval

data Result = Result (RVal, FType) | TypeMismatch | InconsistentTypes deriving Show

run :: Fix ExprF -> Result
run e = case typecheck (typeTransform e) of
    Nothing -> InconsistentTypes
    Just t -> case eval (evalTransform e) of
        Nothing -> TypeMismatch
        Just v -> Result (v, t)

intExpr = Fx $ (Fx $ (Fx $ CInt 2) `Add` (Fx $ CInt 3)) `Mul` (Fx $ CInt 4)

boolExpr = Fx $ (Fx $ (Fx $ CBool False) `Or` (Fx $ CBool True)) `And` (Fx $ CBool True)

eqlExpr = Fx $ (Fx $ (Fx $ CInt 2) `Add` (Fx $ CInt 3)) `Equal` (Fx $ CInt 4)

badExpr = Fx $ (Fx $ CBool False) `Or` (Fx $ CInt 500)

ifExpr = Fx $ (If (Fx $ CBool True) (Fx $ CInt 3) (Fx $ CInt 4))

funExpr = Fx $ Function "x" (Fx $ And (Fx $ CBool True) (Fx $ CVar "x"))

applExpr = Fx $ Appl (Fx $ Function "x" (Fx $ Add (Fx $ CVar "x") (Fx $ CInt 4))) (Fx $ CInt 2)

main = mapM_ print [ run intExpr
                   , run boolExpr
                   , run eqlExpr
                   , run badExpr
                   , run ifExpr
                   , run funExpr
                   , run applExpr
                   ]

