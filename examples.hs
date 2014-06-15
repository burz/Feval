import FVL.FAST
import FVL.Algebra
import qualified FVL.F as F
import qualified FVL.EF as EF

-- (2 + 3) * 4
intExpr = Fx $ (Fx $ (Fx $ CInt 2) `Add` (Fx $ CInt 3)) `Mul` (Fx $ CInt 4)

-- 4 / (3 - 1)
anotherIntExpr = Fx $ (Fx $ CInt 4) `Div` (Fx $ (Fx $ CInt 3) `Sub` (Fx $ CInt 1))

-- (False || True) && True
boolExpr = Fx $ (Fx $ (Fx $ CBool False) `Or` (Fx $ CBool True)) `And` (Fx $ CBool True)

-- !True
notExpr = Fx $ Not (Fx $ CBool True)

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
letExpr = Fx $ EF.Let "x" [] (Fx $ EF.CInt 4) (Fx $ EF.Add (Fx $ EF.CVar "x") (Fx $ EF.CInt 4))

-- 4; True
semiExpr = Fx $ EF.Semi (Fx $ EF.CInt 4) (Fx $ EF.CBool True)

-- Let f x y = x - y In f 4 5
eefLetExpr = let add = Fx $ EF.Sub (Fx $ EF.CVar "x") (Fx $ EF.CVar "y") in
    let innerappl = Fx $ EF.Appl (Fx $ EF.CVar "f") (Fx $ EF.CInt 4) in
    let appl = Fx $ EF.Appl innerappl (Fx $ EF.CInt 5)
    in Fx $ EF.Let "f" ["x", "y"] add appl

-- Let f x y = If x = 0 Then If y = 0 Then 0 Else y + f x (y - 1) Else x + f (x - 1) y In f 3 3
recExpr = let eql s = Fx $ EF.Equal (Fx $ EF.CVar s) (Fx $ EF.CInt 0) in
    let min s = Fx $ EF.Sub (Fx $ EF.CVar s) (Fx $ EF.CInt 1) in
    let tripapp f x y = Fx $ EF.Appl (Fx $ EF.Appl (Fx $ EF.CVar f) x) y in
    let xadd = Fx $ EF.Add (Fx $ EF.CVar "x") (tripapp "f" (min "x") (Fx $ EF.CVar "y")) in
    let yadd = Fx $ EF.Add (Fx $ EF.CVar "y") (tripapp "f" (Fx $ EF.CVar "x") (min "y")) in
    let innerif = Fx $ EF.If (eql "y") (Fx $ EF.CInt 0) yadd in
    let ifstmt = Fx $ EF.If (eql "x") innerif xadd
    in Fx $ EF.Let "f" ["x", "y"] ifstmt (tripapp "f" (Fx $ EF.CInt 3) (Fx $ EF.CInt 3))

intS = "1"

boolS = "True"

varS = "hello"

addS = "4 + 6 + 45"

subS = "784 - 84"

mulS = "5 * 5"

divS = "56 / 32"

andS = "True && False"

orS = "True || False"

notS = "!False"

equalS = "67 = 45"

ifS = "If 5 = 5 Then 5 Else 6"

functionS = "Function x -> x + 56"

applS = "(Function x -> True && x) False"

letS = "Let f x = If x = 0 Then 0 Else x + (f (x - 1)) In f 3"

semiS = "True; 75"

crazyLetS = "Let f x y z = Function w -> If w = 0 Then 0 Else f w x y z In f 1 1 1 0"

intListS = "[0, 1, 2, 5]"

boolListS = "[True, False, False]"

listCaseS = "Case [1, 2, 3, 4, 5] Of [] -> [3, 4] | (x:xs) -> x : xs"

listFunS = "(Function x -> Case x Of [] -> True | (x:xs) -> x && True) [True, False, True]"

main = do
    putStrLn "strict expressions::\n--------------------"
    mapM_ print [ F.run  intExpr
                , F.run  anotherIntExpr
                , F.run  boolExpr
                , F.run  notExpr
                , F.run  eqlExpr
                , F.run  badExpr
                , F.run  ifExpr
                , F.run  funExpr
                , F.run  applExpr
                , F.run  letRecExpr
                , F.run  twoArgRecExpr
                , EF.run letExpr
                , EF.run semiExpr
                , EF.run recExpr
                , EF.run eefLetExpr
                ]
    putStrLn "parsed expressions::\n--------------------"
    mapM_ print [ EF.parseRun intS
                , EF.parseRun boolS
                , EF.parseRun varS
                , EF.parseRun addS
                , EF.parseRun subS
                , EF.parseRun mulS
                , EF.parseRun divS
                , EF.parseRun andS
                , EF.parseRun orS
                , EF.parseRun notS
                , EF.parseRun equalS
                , EF.parseRun ifS
                , EF.parseRun functionS
                , EF.parseRun applS
                , EF.parseRun letS
                , EF.parseRun semiS
                , EF.parseRun crazyLetS
                , EF.parseRun intListS
                , EF.parseRun boolListS
                , EF.parseRun listCaseS
                , EF.parseRun listFunS
                ]

