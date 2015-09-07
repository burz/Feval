Feval - evaluation using f-algebras
===================================

Author: Anthony Burzillo

******

Feval is a statically typed functional programming language that uses f-algebras as opposed to classic recursion to solve
the problem of evaluation and typechecking, which allows the compiler to perform better optimizations.

The overall language of Feval is EF, which is an extension of the smaller language F. In order to run a program written
in EF, we first use an f-algebra to transform the AST to F, and then we transform the result (via an f-algebra) into
the individual AST's used by the evaluator's f-algebra and the typechecker's f-algebra. The reason we need to use
seperate f-algebras is because we need to stall the processing of certain subtrees of the AST in order to ensure correct
evaluation and typechecking. For instance, we cannot evaluate the expression of a anonymous function until we have
obtained its argument. Similarly, we cannot typecheck a function until we have assigned a type hypothesis to the variable.

For more information on how we solve these problems, check out the article [Feval: F-Algebras for expression evaluation](http://burz.github.io/2014/06/15/feval.html). For an in-depth explanation of parsing see [Feval: Parsing a functional language with Parsec](http://burz.github.io/2014/06/24/parsing.html)

## Usage

To build Feval run `cabal configure && cabal build && cabal install`.
Then you can run `Feval` which acts as a REPL:
```
$ ./feval
Function x -> x && True
  => Function x -> x && True
    : Bool -> Bool
(Function x -> Function y -> x + y / 50) 5
  => Function y -> 5 + y / 50
    : Int -> Int
Let f x = If x = 0 Then 1 Else x * f (x - 1) In f 6
  => 720
    : Int
Case [1, 2, 3, 4] Of [] -> 0 | (x : xs) -> x + 6
  => 7
    : Int
Function x -> Case True : x Of [] -> True | (y : ys) -> True || !(y || False)
  => Function x -> Case True Of [] -> True | (y, ys) -> True || !(y || False)
    : [Bool] -> Bool
```
To quit simply press ctrl-d.

Since the REPL can only handle one line expressions, we also allow `feval` to take a file to execute as an argument.
For instance, with the provided file mergesort.fvl:
```
$ feval mergesort.fvl
  => [-34, 3, 4, 23]
    : [Int]
```

## Expressions

### Boolean Operations

We allow conjunction (&&), disjunction (||), and negation (!) expressions.

### Integer Operations

The operations of addition (+), subtraction (-), multiplication (*), division (/), and modulus (%) evaluate to integer
values. On the other hand, the comparison operators of equality (=), less-than (<), less-than-or-equal (<=), greater-than
(>), and greater-than-or-equal (>=) all evaluate to boolean values.

### Functional Operations

We allow the creation of anonymous functions via `Function x -> e` where `e` is some expression, and `x` is the argument
to the function. We can create multiple argument anonymous functions via `Function x -> Function y -> e` where `x` is
the first argument and `y` is the second, etc.

To apply a function `f` simply use `f e` where `e` is the expression for the first argument, or `f e1 e2` for the first
argument `e1` and second argument `e2`.

### If Expressions

Use `If e1 Then e2 Else e3`.

### Let Expressions

We allow let expressions to define constants and functions (possibly recursive) via
```
Let x = 4 In x + 54
```
and
```
Let f x y = If x = 0 Then 0 Else y + f (x - 1) y In f 3 4
```

### Semi-colon Expressions

An expression of the form `e1; e2` first evaluates `e1` then `e2` and returns the result of `e2`.

### List Expressions

You can create empty lists `[]` or lists with values `[1, 2, 3, 4]`. You can cons values onto a list via
```
5 : [4, 5, 6]
  => [5, 4, 5, 6] 
    : [Int]
```
Finally, we can match on lists via a case expression via
```
Case e1 Of [] -> e2 | (x : xs) -> e3
```
