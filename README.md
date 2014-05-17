feval - evaluation using f-algebras
===================================

Author: Anthony Burzillo

******

feval is a functional programming language evaluator that uses f-algebras as opposed to classic recursion to solve
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
