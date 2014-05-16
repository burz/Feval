import AST
import Algebra
import Type
import Eval

data Result = Result (CVal, FType) | TypeError deriving Show

run :: Fix ExprF -> Result
run e = case typecheck e of
    Nothing -> TypeError
    Just t -> Result (eval e, t)

intExpr = Fx $ (Fx $ (Fx $ Const (CInt 2)) `Add`
               (Fx $ Const (CInt 3))) `Mul` (Fx $ Const (CInt 4))

boolExpr = Fx $ (Fx $ (Fx $ Const (CBool False)) `Or`
                (Fx $ Const (CBool True))) `And` (Fx $ Const (CBool True))

eqlExpr = Fx $ (Fx $ (Fx $ Const (CInt 2)) `Add`
               (Fx $ Const (CInt 3))) `Equal` (Fx $ Const (CInt 4))

badExpr = Fx $ (Fx $ Const (CBool False)) `Or` (Fx $ Const (CInt 500))

ifExpr = Fx $ (If (Fx $ Const (CBool True)) (Fx $ Const (CInt 3))
                 (Fx $ Const (CInt 4)))

main = mapM_ print [ run intExpr
                   , run boolExpr
                   , run eqlExpr
                   , run badExpr
                   , run ifExpr
                   ]

