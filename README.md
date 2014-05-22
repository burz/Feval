Feval - evaluation using f-algebras
===================================

Author: Anthony Burzillo

******

Feval is a functional programming language evaluator that uses f-algebras as opposed to classic recursion to solve
the problem of evaluation and typechecking.

For an example, in the language of feval one can represent the following function

```
(Function x -> x + 4)
```

as (in Haskell)

```
func = Fx $ Function "x" (Fx $ Add (Fx $ CVar "x") (Fx $ CInt 4))
```

We can evaluate this function by running

```
run func
=> Result (Function x -> x + 4,Int -> Int)
```

which says that the result of evaluating the expression is a function with type Int -> Int.

(A good introduction to f-algebras is found [here](https://www.fpcomplete.com/user/bartosz/understanding-algebras))

******

In order to compile EFeval, or Extended Feval, which includes exprssions like `Let x x ... x = e In e`
(possibly recursive) or `e; e`, we transform it via an f-algebra to the smaller language Feval which includes only
function application and LetRec, which allows for recursion. Similarly both eval and typecheck use their own f-algebras
using types which allow us to delay the evaluation of certain parts of the AST until needed.

Check out [GRAMMAR.txt](GRAMMAR.txt) to see what kinds of expressions each of EFeval and Feval can define, as well as
[examples.hs](examples.hs) for some example expressions.

For ease of use at the moment the expressions of EFeval and FEval are instances of Show so

```
Fx $ Function "x" (Fx $ Add (Fx $ CVar "x") (Fx $ CInt 4))
=> Function x -> x + 4
```
