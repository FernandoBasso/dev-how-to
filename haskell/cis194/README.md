# cis194

https://www.seas.upenn.edu/~cis194/spring13/lectures.html





## 01 Intro

[Link to Lecture 1](https://www.seas.upenn.edu/~cis194/spring13/lectures/01-intro.html)



C/Java/C++/C#/etc

```java
int acc = 0;
for ( int i = 0; i < lst.length; i++ ) {
  acc = acc + 3 * lst[i];
}
```
vs

Haskell:

```haskell
sum (map (3*) lst)
```



`x :: Int` means “x has type int”.

Comments:

```haskell
-- single line comment
{-
multi
line comment
-}
```

Haskell variables are immutable. `x = 3` does not mean “3 is assigned to x” but “x is defined to be 4”. `=` denotes definition, not assignment.



@TODO: Is this an infinite thing?

```haskell
y :: Int
y = y + 1
```

`maxBound` and `minBound`

```ghci
λ let numDigits = length (show (2 ^ ( 2 ^ (2 ^ (2 ^ 2)))))
λ numDigits 
19729
```



IMPORTANT gotcha about GHCI:

https://stackoverflow.com/questions/15978118/haskell-basic-factorial-not-exiting







