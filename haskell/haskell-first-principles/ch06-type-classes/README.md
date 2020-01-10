# Type Classes - Chapter 06

## What are type classes

Type declaration defines how a type is created, a type class defines how a set of types are consumed or used. See [Philip Wadler, The Expression Problem](http://homepages.inf.ed.ac.uk/wadler/papers/expression/expression.txt).

Type classes are like interfaces in other languages.

If a type implements an instance of a certain type class, it is possible to use a set of operations of that type class on a value of that type. It means that there is code that defines how the values and functions from that type class work for that type.

## Back to Bool

`:info Bool` and see which type classes `Bool` has instances of, like `Eq`, `Ord`, and others. Type classes have some sort of hierarchy because some things depend upon others, like, to be ordered, things need to be comparable, etc.

## Eq

The function type does not have an instance of `Eq`. A lot of types (`:info Eq`) have an instance of `Eq` and therefore we can compare data of those types.

`:info (,)` (the tuple constructor) shows that tuples can be compared:

```
λ> (1, 1) == (1, 1)
True
λ> (1, 1) == (1, 2)
False
λ> (1, 1) /= (1, 1)
False
λ> (1, 1) /= (1, 2)
True
```

`==` is very “generic” (as is `/=`). If we partially apply `==` to a concrete type, it specializes to that specific type:

```ghci
λ> :type (==)
(==) :: Eq a => a -> a -> Bool

λ> :type (==) "foo"
(==) "foo" :: [Char] -> Bool

λ> :type (==) (9 :: Double)
(==) (9 :: Double) :: Double -> Bool
```

Now, `(==) "foo"` expects `[Char]` (not `Eq a` any longer) and returns the `Bool` (the return type doesn’t and CAN’T change). A similar thing happens with the double example. Not that `Eq` is dropped after it gets more specific because it is no longer necessary; Both `[Char]` and `Double` already have instances of `Eq`.

## Writing type class instances



## Ord



## Enum



## Show



## Read



## Instances are dispatched by type



## Gimme more operations



## Chapter Exercises



## Chapter Definitions



## Type class inheritance, partial

