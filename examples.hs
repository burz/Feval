import AST
import Algebra
import qualified Feval as F
import qualified EFeval as EF

-- (2 + 3) * 4
intExpr = Fx $ (Fx $ (Fx $ CInt 2) `Add` (Fx $ CInt 3)) `Mul` (Fx $ CInt 4)

-- 4 / (3 - 1)
anotherIntExpr = Fx $ (Fx $ CInt 4) `Div` (Fx $ (Fx $ CInt 3) `Sub` (Fx $ CInt 1))

-- (False || True) && True
boolExpr = Fx $ (Fx $ (Fx $ CBool False) `Or` (Fx $ CBool True)) `And` (Fx $ CBool True)

-- (2 + 3) = 4
eqlExpr = Fx $ (Fx $ (Fx $ CInt 2) `Add` (Fx $ CInt 3)) `Equal` (Fx $ CInt 4)

-- False || 500
badExpr = Fx $ (Fx $ CBool False) `Or` (Fx $ CInt 500)

-- If True Then 3 Else 4
ifExpr = Fx $ (If (Fx $ CBool True) (Fx $ CInt 3) (Fx $ CInt 4)) 

-- Function x -> True && x
funExpr = Fx $ Function "x" (Fx $ And (Fx $ CBool True) (Fx $ CVar "x"))

-- (Function x -> x + 4) 2
applExpr = Fx $ Appl (Fx $ Function "x" (Fx $ Add (Fx $ CVar "x") (Fx $ CInt 4))) (Fx $ CInt 2)

-- Let Rec f x = If x = 0 Then 0 Else x + f (x - 1) In f 3
letRecExpr = let eql = Fx $ Equal (Fx $ CVar "x") (Fx $ CInt 0) in
    let appl = Fx $ Appl (Fx $ CVar "f") (Fx $ Add (Fx $ CVar "x") (Fx $ CInt (-1))) in
    let els = Fx $ Add (Fx $ CVar "x") appl in
    let ifstmt = Fx $ If eql (Fx $ CInt 0) els
    in Fx $ LetRec "f" "x" ifstmt (Fx $ Appl (Fx $ CVar "f") (Fx $ CInt 3))

-- Let Rec f x = Function y -> If x = 0 Then If y = 0 Then 0 Else y + f x (y - 1) Else x + f (x - 1) y
-- In f 3 3
twoArgRecExpr = let eql s = Fx $ Equal (Fx $ CVar s) (Fx $ CInt 0) in
    let min s = Fx $ Sub (Fx $ CVar s) (Fx $ CInt 1) in
    let tripapp f x y = Fx $ Appl (Fx $ Appl (Fx $ CVar f) x) y in
    let xadd = Fx $ Add (Fx $ CVar "x") (tripapp "f" (min "x") (Fx $ CVar "y")) in
    let yadd = Fx $ Add (Fx $ CVar "y") (tripapp "f" (Fx $ CVar "x") (min "y")) in
    let innerif = Fx $ If (eql "y") (Fx $ CInt 0) yadd in
    let ifstmt = Fx $ If (eql "x") innerif xadd in
    let fun = Fx $ Function "y" ifstmt
    in Fx $ LetRec "f" "x" fun (tripapp "f" (Fx $ CInt 3) (Fx $ CInt 3))

-- Let x = 4 In x + 4
letExpr = Fx $ EF.Let "x" (Fx $ EF.CInt 4) (Fx $ EF.Add (Fx $ EF.CVar "x") (Fx $ EF.CInt 4))

-- 4; True
semiExpr = Fx $ EF.Semi (Fx $ EF.CInt 4) (Fx $ EF.CBool True)

main = mapM_ print [ F.run  intExpr
                   , F.run  anotherIntExpr
                   , F.run  boolExpr
                   , F.run  eqlExpr
                   , F.run  badExpr
                   , F.run  ifExpr
                   , F.run  funExpr
                   , F.run  applExpr
                   , F.run  letRecExpr
                   , F.run  twoArgRecExpr
                   , EF.run letExpr
                   , EF.run semiExpr
                   ]

