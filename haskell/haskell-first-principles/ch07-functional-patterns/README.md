# More Functional Patterns - Chapter 07 - Haskell From First Principles



[mvaldesdeleon ch07 solutions](https://github.com/mvaldesdeleon/haskell-book/blob/master/ch07/exercises.md)



## Make it func-y

**Function**: instruction to produce output from an input. Functions are applied t o arguments which binds their parameters to values. The fully applied function with its arguments is then evaluated to produce the output or result.

Haskell functions are first-class entities that: 

- can be values in expressions, lists, or tuples;
- can be passed as arguments to a function;
- can be returned from a function as a result;
- make use of syntactic patterns.



## Arguments and parameters

Functions may appear to take multiple arguments, but that is just syntax sugar. Functions in Haskell always take only a single a argument and return one result. Functions are _defined_ by the fact that they can be applied to an argument to produce a result.

All Haskell values can be arguments to functions. A value that can be used as an argument to a function is a _first-class_ value. Functions are first-class values in Haskell. 

```haskell
myNum :: Integer
myNum = 1
```

`myNum` _is_ an Integer of the value 1.

```haskell
myVal f = f 5
```

`myVal` has to be a function because it takes `f`, and `f` itself has to be a function because we see it is applied to `5`.

`myVal f g h = ...`  implies nested functions, each applied to one argument, returning a function which is applied to the next argument, and so on. It just _looks like_ several arguments at the term level.

### Binding variables to values

When we apply the function `f x = x + 1` to the value 5, we say that the value x is bound to the value 5 and `x` means 5 inside the function. One cannot use the result of function unless it is first applied.

LEXICAL (static) SCOPE: scope depends on where in the file/code a value/variable is defined.

## Anonymous Functions





## Pattern Matching





## Case expressions





## Higher-order functions





## Guards





## Function composition





## Pointfree style





## Demonstrating composition





