import AST
import Algebra
import qualified Feval as F
import qualified EFeval as EF

intExpr = Fx $ (Fx $ (Fx $ CInt 2) `Add` (Fx $ CInt 3)) `Mul` (Fx $ CInt 4)

boolExpr = Fx $ (Fx $ (Fx $ CBool False) `Or` (Fx $ CBool True)) `And` (Fx $ CBool True)

eqlExpr = Fx $ (Fx $ (Fx $ CInt 2) `Add` (Fx $ CInt 3)) `Equal` (Fx $ CInt 4)

badExpr = Fx $ (Fx $ CBool False) `Or` (Fx $ CInt 500)

ifExpr = Fx $ (If (Fx $ CBool True) (Fx $ CInt 3) (Fx $ CInt 4)) 

funExpr = Fx $ Function "x" (Fx $ And (Fx $ CBool True) (Fx $ CVar "x"))

applExpr = Fx $ Appl (Fx $ Function "x" (Fx $ Add (Fx $ CVar "x") (Fx $ CInt 4))) (Fx $ CInt 2)

letExpr = Fx $ EF.Let "x" (Fx $ EF.CInt 4) (Fx $ EF.Add (Fx $ EF.CVar "x") (Fx $ EF.CInt 4))

semiExpr = Fx $ EF.Semi (Fx $ EF.CInt 4) (Fx $ EF.CBool True)

main = mapM_ print [ F.run  intExpr
                   , F.run  boolExpr
                   , F.run  eqlExpr
                   , F.run  badExpr
                   , F.run  ifExpr
                   , F.run  funExpr
                   , F.run  applExpr
                   , EF.run letExpr
                   , EF.run semiExpr
                   ]

