import AST
import Algebra
import Eval

intExpr = Fx $ (Fx $ (Fx $ Const (CInt 2)) `Add`
               (Fx $ Const (CInt 3))) `Mul` (Fx $ Const (CInt 4))

boolExpr = Fx $ (Fx $ (Fx $ Const (CBool False)) `Or`
                (Fx $ Const (CBool True))) `And` (Fx $ Const (CBool True))

eqlExpr = Fx $ (Fx $ (Fx $ Const (CInt 2)) `Add`
               (Fx $ Const (CInt 3))) `Equal` (Fx $ Const (CInt 4))

badExpr = Fx $ (Fx $ Const (CBool False)) `Or` (Fx $ Const (CInt 500))


main = mapM_ print [ eval intExpr
                   , eval boolExpr
                   , eval eqlExpr
                   , eval badExpr
                   ]

