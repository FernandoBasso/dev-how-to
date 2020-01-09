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

[mvaldesdeleon](https://github.com/mvaldesdeleon/haskell-book/blob/master/ch05/exerises.md#parametricity)

1. There is nothing we can do about it. Impossible.

2. If `a -> a` is the identity function and the only thing it can do is to return its argument, then `a -> a -> a` has two possible implementations only, which is to return the first argument, or the second, similar to what `id` does. We end up with implementations similar to what `fst` and `snd` for tuples do.

   ```haskell
   f1 :: a -> a -> a
   f1 x y = x
   
   f2 :: a -> a -> a
   f2 x y = y
   
   --
   -- λ> f1 "Tomb" "Raider"
   -- "Tomb"
   -- λ> f2 "Tomb" "Raider"
   -- "Raider"
   --
   ```

3. It can have only one implementation, because it is again similar to the `id` function. Since `a -> b -> b` is parametricaly polymorphic, there is nothing it can do but to return `b`.

   ```haskell
   h :: a -> b -> b
   h x y = y
   
   --
   -- λ> h "Tomb Raider" "The Last Revelation"
   -- "The Last Revelation"
   --
   ```

   

## Exercises: Apply Yourself

Start at end of page 146.

```ghci
λ> :t (++)
(++) :: [a] -> [a] -> [a]
```

All we know about `++` is that it takes "two" lists of some type `a` and produces a list  of some type `a`.

### 1 list concatenation ++

The type signature of `++`: `(++) :: [a] -> [a] -> [a]`. It is only known that it concatenates lists of some type `a`. However, `myConcat` now _has_ not alternative but to infer that it will concatenate lists of `Char` because of the literal string `" yo Adrian!"`.

```ghci
λ> myConcat s = s ++ " yo Adrian!"
λ> :t myConcat
myConcat :: [Char] -> [Char]
```

### 2 general function *

The function `*` has the signature signature `(*) :: Num a => a -> a -> a`. When used in the expression `(x / 3) * 5` _has_ to produce some sort of fractional type:

```ghci
λ> myMult x = (x / 3) * 5
λ> :t myMult
myMult :: Fractional a => a -> a
```



### 3 take

`take` has the signature `take :: Int -> [a] -> [a]` . If we create a function that “takes” from a list of chars, the return type must be `[Char]`.

```ghci
λ> myTake n = take n "Master Yoda"
λ> :t myTake
myTake :: Int -> [Char]
```



### 4 Ord, Int and >

```ghci
λ> :t (>)
(>) :: Ord a => a -> a -> Bool

λ> myCom n = n > (length [1..10])
λ> :t myCom
myCom :: Int -> Bool
```

`n` is parametric polymorphic, meaning it could be any type, or any of the number types, but since `length` returns an `Int`, `n` has to be turned into an `Int` too.



### 5 Ord, Char and <

```ghci
λ> :t (<)
(<) :: Ord a => a -> a -> Bool

λ> myAlph c = c < 'k'
λ> :t myAlph
myAlph :: Char -> Bool
```

Since inside the function body `c` is being compared with a `'k'`, clearly a `Char`, then the `c` parameter must also be a `Char`, otherwise the comparison would be impossible.







