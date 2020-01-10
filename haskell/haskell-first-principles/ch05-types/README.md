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

## Chapter Exercises

### Multiple choice

1. C is correct.
2. A is correct. The outer `[ ]` means “a list of something”. The inner `[a]` means “a list of some type _a_, which could be a list of chars. So, `[[a]]` could be `[[Char]]` or `[String]`.
3. B is correct. It takes a list of some type _a_, an `Int` (probably an index), and returns one element of the type _a_ from the list.
4. C is correct.

### Determine the type

As instructed in the book, add this to the top of the file:

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}
```



#### Exercise 1a

```ghci
val = (* 9) 6
λ> val1
54
λ> :t val1
val1 :: Num a => a
```

#### Exercise 1b

```haskell
val = head[(0, "Lara"), (1, "Croft")]
```

```λ> val
λ> val
(0,"Lara")
λ> :t val
val :: Num a => (a, [Char])
```

#### Exercise 1c

```haskell
val = head[(0 :: Integer, "Lara"), (1, "Croft")]
```

```ghci
λ> val
(0,"Lara")
λ> :t val
val :: (Integer, [Char])
```

#### Exercise 1d

```haskell
val = if False then True else False
```

```ghci
λ> val
False
λ> :t val
val :: Bool
```

#### Exercise 1e

```haskell
val = length [1..5]
```

```ghci
λ> val
5
λ> :t val
val :: Int
```

Since `length` returns an `Int`, the type of `val` _has to be_ an `Int`.

#### Exercise 1f

```haskell
val = (length [1..4]) > (length "TACOCAT")
```

```ghci
λ> val
False
λ> :t val
val :: Bool
```

### Exercise 2

There is not enough information for the concrete type of number, so the compiler infers the most generic, the type class `Num`, therefore, the type of `w` is `Num a`.

### Exercise 3

```haskell
x = 5
y = x + 5
z y = y * 10
```

```ghci
λ> :t z
z :: Num a => a -> a
```

### Exercise 4

```haskell
x = 5
y = x + 5
f = 4 / y
```

```ghci
λ> :t f
f :: Fractional a => a
```

### Exercise 5

```haskell
x = "Julie"
y = " <3 "
z = "Haskell"
f = x ++ y ++ z
```

```ghci
λ> :t f
f :: [Char]
```

Yes, just `[Char]` because `f` does not take parameters, and just uses `++` with strings.

### Does it compile?

#### Exercise 1

```haskell
bigNum = (^) 5 $ 10
wahoo = bigNum $ 10
```

The `$ 10` on line one is wrong. Fix:

```haskell
bigNum = (^) 5
wahoo = bigNum $ 10
```

```ghci
λ> bigNum 5
3125
λ> bigNum $ 5
3125
```

#### Exercise 2

```haskell
x = print
y = print "tomb"
z = x "raider"
```

```ghci
λ> z
"raider"
```

Yes, it does compile. Functions are values. We assigned `print` to `x` and now `x` is the function `print`. `z` is a function that uses `x` (`print`) to print the string.

#### Exercise 3

They don't tell what the expected result should be. There are several ways to "fix" this. One is:

```haskell
a = (+)
b = a
c = b 10
d = c 200
```

#### Exercise 4

`c` is not in scope. We must do something like `c = 1`.

```haskell
c = 1
a = 12 + b
b = 10000 * c
```

```ghci
λ> b
10000
λ> a
10012
```

### Type variable or specific type constructor?

The answers are one of:

- fully polymorphic type variable
- constrained polymorphic type variable
- concrete type constructor



#### Exercise 1

```haskell
f :: Num a => a -> b -> Int -> Int
--           [0]  [1]    [2]    [2]
```

Constrained polymorphic [1], fully polymorphic [2], concrete [2] and [3].

#### Exercise 2

```haskell
f :: zed -> Zed -> Blah
--   [1]    [2]     [3]
```

Fully polymorphic type variable [1], concrete type constructor [2] and [3].

#### Exercise 3

```haskell
f :: Enum b => a -> b -> C
--            [1]  [2]  [3]
```

Fully polymorphic type variable [1], constrained polymorphic type [2], concrete [3]. `C` is an uppercase letter, like a type for `Integer` or `Double`.

#### Exercise 4

```haskell
f :: f -> g -> C
--  [1]  [2]  [3]
```

Fully polymorphic type variable [1] and [2], concrete [3].

### Write a type signature

#### Exercise 1

```fnH :: [a] -> a
fnH :: [a] -> a
fnH (x:_) = x
```

#### Exercise 2

```haskell
fnC :: (Ord a) => a -> a -> Bool
fnC x y =
  if (x > y) then True else False
```

Note that we need only _one_ constrained type `Ord a`. The `>` operator need both operands to be of the same type, so, the two parameters for the function are of the same constrained type `a`.

#### Exercise 3

```haskell
fnS :: (a, b) -> b
fnS (x, y) = y
```



### Given a type, write a function

#### Exercise 1











