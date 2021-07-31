# Rewriting functions using folds

- [Rewriting functions using folds](#rewriting-functions-using-folds)
  - [and](#and)
    - [Direct recursion, not point-free](#direct-recursion-not-point-free)
    - [Folding and point-free](#folding-and-point-free)
  - [or](#or)
    - [Direct recursion, not point free](#direct-recursion-not-point-free-1)
    - [Direct recursion, point-free](#direct-recursion-point-free)
  - [any](#any)
    - [Direct recursion, not point-free](#direct-recursion-not-point-free-2)
    - [Type checking problem](#type-checking-problem)
    - [Composing with f and map](#composing-with-f-and-map)
    - [Composing (||) and f](#composing-and-f)

## and

### Direct recursion, not point-free

```
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd_r xs
```

Using `foldr`, not point free.
```
myAnd :: [Bool] -> Bool
myAnd = foldr (\e acc -> e && acc) True
```

### Folding and point-free

`(&&)` is the folding function and `True` is the *zero* (initial value for the accumulator). We partially apply `foldr` with the first two params. The remaining one, the list to be operated on is still missing, and it is not defined as a parameter to `myAnd`. There is no `myAnd xs = ...`. That is what makes it *point free*. The type signature denounces that wee need a list of bool, though, and return a bool.

```
myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True
```

## or

### Direct recursion, not point free

```
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs
```

@TODO: Why does the author say it is **not** point free? We are not declaring that `myOr` explicitly takes a list, like `myOr xs = ...`.

To me, this is point-free.

```
myOr :: [Bool] -> Bool
myOr = foldr (\e acc -> e || acc) False
```

Or, the folding function is not point-free, because it takes explicit params `e` and `acc`.

This is not point free because we are explicitly declaring the param `xs`.
```
myOr :: [Bool] -> Bool
myOr xs = foldr (\e acc -> e || acc) False xs
```

### Direct recursion, point-free

And the fully point-free version using folds.
```
myOr :: [Bool] -> Bool
myOr = foldr (||) False
```

“Point” in *point-free* refers to variables. Variables are the *points*. If a function mentions variables, it is not point-free.

## any

### Direct recursion, not point-free

```
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs
```

### Type checking problem

This version works if we comment the type signature for the inner function `g`. That type definition seems totally OK but it doesn't type check.

```
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr g False
  where
    -- g :: a -> Bool -> Bool
    g = \ x acc -> f x || acc
```

In the  definition of `g` we reference a variable that appears in the signature of `myAny`. But those `a`s are different. We could enable `ScopedTypeVariables` extension, though, which makes those `a`'s be the same.

Also, we see a compiler error mentioning `a1`, which is the name to compiler gives to the `a` polymorphic type variable in `g` to differentiate it from the `a` polymorphic type variable in `myAny`. This works:

```
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}

myAny :: forall a. (a -> Bool) -> [a] -> Bool
myAny f = foldr g False
  where
    g :: a -> Bool -> Bool
    g = \ e acc -> f e || acc
```

Thanks @infixl-1 (Izuzu#5593) for the [help on Discord](https://discord.com/channels/280033776820813825/796099254937845790/871017722761322516). He mentioned that “I usually reserve type signatures for top-level declarations because of shenanigans like this (which honestly aren't really shenanigans).”

### Composing with f and map

```
myAny :: (a -> Bool) -> [] a -> Bool
myAny f = foldr (||) False . map f
```

We can think of it like this:

We first `map f` over the list of values. Ex:

```
λ> map even [0, 2, 3]
[True,True,False]
```

Then, we `(||)` each on the results of the previous operation:

```
λ> foldr (||) False [True, True, False]
True
```

*EXCEPT THAT BECAUSE OF LAZY EVALUATION, `map f` will only produce values as as needed by `foldr`*. It WILL NOT actually map over the entire input unless `foldr` folding function keeps asking for more values and ends up reaching the base case. As soon as `f` produces `True` for an element, `myAny` returns `True` and no more of the list is processed.

Thanks @konsumlamm for [this answer on Discord](https://discord.com/channels/280033776820813825/796099254937845790/871041627060334632).

### Composing (||) and f

This is a suggestions from the liter. Easier to understand!

```
myAny :: (a -> Bool) -> [] a -> Bool
myAny f = foldr ((||) . f) False
```
