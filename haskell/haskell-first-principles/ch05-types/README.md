# Types - Chapter 05



## Exercises on Type Arguments

Starts at page 137.

### Exercise 01

```ghci
λ> f :: a -> a -> a -> a ; f = undefined
λ> x = 'x'
λ> :t f x
f x :: Char -> Char -> Char
```



### Exercise 02
```ghci
λ> g :: a -> b -> c -> b ; g = undefined
λ> :t g 0 'c' "woot"
g 0 'c' "woot" :: Char
```



### Exercise 03
```ghci
λ> h :: (Num a, Num b) => a -> b -> b; h = undefined
λ> :t h 1.0 2
h 1.0 2 :: Num b => b
```



`Num b => b` instead of `Double` because until 1.0 has to be decided upon a concrete type, it could be `Float`, `Double`, etc., so `Num b` is the best the compiler can do at this point since `Num` type class encompasses all the numeric types.

### Exercise 04
```ghci
λ> h :: (Num a, Num b) => a -> b -> b; h = undefined
λ> :t h 1 (1.1 :: Double)
h 1 (1.1 :: Double) :: Double
```

Now the type is `Double` (not `Num b` like in the previous exercise) because we explicitly told the compiler that 1.1 is of type `Double`. Now the compiler knows that `b` is `Double` and doesn’t have to be generic about it.

### Exercise 05

```ghci
λ> jackal :: (Ord a, Eq b) => a -> b -> a; jackal = undefined
λ> :t jackal "foo" "bar baz"
jackal "foo" "bar baz" :: [Char]
```

When `"foo"` is passed to `jackal` as the *a* argument, at that point, *a* is known to have a concrete type of `[Char]`, therefore, the return *a* type must be `[Char]`.

### Exercise 06

```ghci
λ> jackal :: (Ord a, Eq b) => a -> b -> a; jackal = undefined
λ> :t jackal "Tomb Raider 1996"
jackal "Tomb Raider 1996" :: Eq b => b -> [Char]
```

We just passed the parameter for *a*. Still have to provide one for *b*, which is `Eq  b => b`. Yet, the return type is `[Char]` because *a* was passed a value of that type `[Char]`.

### Exercise 07 (???)

```ghci
λ> kessel :: (Ord a, Num b) => a -> b -> a; kessel = undefined
λ> :t kessel 1 2
kessel 1 2 :: (Ord a, Num a) => a
```

@TODO: Why is the result `(Ord a, Num a) => a`? 1 was passed for `a`, so, shouldn't the result be `Num a => a` since the return in the type definition is `a`?

### Exercise 08 (???)

```ghci
λ> kessel :: (Ord a, Num b) => a -> b -> a; kessel = undefined
λ> :t kessel 1 (2 :: Integer)
kessel 1 (2 :: Integer) :: (Ord a, Num a) => a
```

Why isn't the result `Integer` or `Num a => a`? Can't the compiler at this moment infer the type of 1 and then the result could be the type of either parameter?



### Exercise 09

```ghci
λ> kessel :: (Ord a, Num b) => a -> b -> a; kessel = undefined
λ> :t kessel (1 :: Integer) 2
kessel (1 :: Integer) 2 :: Integer
```

`Integer` because since one has the concrete type `Integer` for `a`, the compiler knows that `Num b` has to be an `Integer` too, and the result `a` can't possibly be anything other than `Integer`.



## Exercises: Parametricity



