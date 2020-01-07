---
*description*: Notes from the book Haskell From First Principles
---

# Haskell From First Principles (notes)



## 01 All You Need is Lambda λ



Lambda Calculus, a model of computation devised in the 1930s by Alonzo Church. A calculus is a method of calculation or reasoning; the lambda calculus is one process for formalizing a method. Like Turing machines, the lambda calculus formalizes the concept of effective computability, thus determining which problems, or classes of problems can be solved.

Some languages in this general category incorporate features into the language that aren’t translatable into lambda expressions. Haskell is a pure functional language because it does not.

Some functional programming languages incorporate features that aren't translatable into lambda expressions, there fore, they are not *pure*. Haskell is a *pure* functional programming language because it does not incorporate features that can't be translated into λ-expressions.

purity == referential transparency

Referential transparency means that the same function, given the same values to evaluate, will always return the same result in pure functional programming, as they do in math.

function is a mapping input(s) to output.

input set is the domain

set of possible outputs is called the codomain

Lambda calculus has tree three basic components, or *lambda terms*: expressions, variables, and abstractions.



## 02 Hello Haskell

Install [stack](https://docs.haskellstack.org/en/stable/README/):

```shell-session
$ curl -sSL https://get.haskellstack.org/ | sh
```

To install a given version of Haskell without a “project”, simply do:

```shell-session
$ stack --resolver lts-12.10 ghci
```

And stack will install it if not already installed! These dependencies may need to be installed on your distro: gcc, make, libffi, zlib, libgmp, libtinfo.

Then add this to `~/.stack/global-project/stack.yaml` (because this is the version used by the book, and we don't want unnecessary trouble as we are getting started):

```yaml
resolver: lts-12.10
packages: []
```



```
$ $ stack --resolver lts-12.10 ghci

:set prompt "λ "

λ :quit
λ :q

λ :set prompt "λ "
```

Prelude is in Haskell [base](https://www.stackage.org/package/base).

`:quit` is not Haskell code, but but a GHCi feature.

```ghci
λ :load b01-say-hello.hs
[1 of 1] Compiling Main             ( b01-say-hello.hs, interpreted )
Ok, one module loaded.
λ :info sayHello
sayHello :: String -> IO ()     -- Defined at b01-say-hello.hs:2:1
λ sayHello "Haskell"
Hello, Haskell!
λ :module
λ :info sayHello

<interactive>:1:1: error: Not in scope: ‘sayHello’
```

Can use `:reload` or `:r` to reload when a loaded file has been modified.

Return to Prelude after having loaded a module, type `:module` or `:m`. It unloads the previously loaded file, and its code will no longer be in scope.

We say that expressions are in normal form when there are no more evaluation steps that can be taken, or, put differently, when they’ve reached an irreducible form. Reducible expressions are also called *redexes*.

I had a run-in with the [boss](https://dictionary.cambridge.org/us/dictionary/english/boss)/the [law](https://dictionary.cambridge.org/us/dictionary/english/law)/the [police](https://dictionary.cambridge.org/us/dictionary/english/police) [yesterday](https://dictionary.cambridge.org/us/dictionary/english/yesterday).

Haskell uses a “two pass” compilation. Order of code in a source file does not matter.



What the poop‽

```ghci
λ (\f -> (2, 2 + f)) 2
(2,4)

λ (\f -> (2, 2 + f)) 8
(2,10)

λ (\f -> ('_', 2 + f)) 5
('_',7)
```



Formal parameter is what is defined as a variable in function signature/definitions. Arguments are the actual values we pass when applying functions.

### div, quot, rem, mod, divMod, quotRem



https://stackoverflow.com/questions/8111120/integral-operators-quot-vs-div

https://wiki.haskell.org/Let_vs._Where



## 03 Strings

```ghci
Prelude> :type "Cati"
"Cati" :: [Char]
```

`String` is a type alias (or synonym) for a list of `Char`.



`Foldable t => t [a]` can be thought of as a list, like `[[a]]`.

`[]` is the list type constructor.



## 04 Basic Datatypes

Datatypes, type constructors, data constructors.

Type declarations are how types are defined.

Type constructors are capitalized and are used to write type signatures at the *type level* of the code.

Data constructors are the actual values in the code, at the *term level*.

In `data Bool = False | True` the `|` indicates a *sum type*, or logical disjunction “or”.

https://wiki.haskell.org/Constructor



```haskell
data Bool = False | True
```

the `|` indicates a *sum type*, or *logical disjunction* “or”.

![image-20200106144738511](/home/fernando.basso/Projects/proghowto/haskell/haskell-first-principles/images/data-declaration1.png)





The whole thing is a **data declaration**.

`Bool` is a type constructor, but True and False are data constructors.

`:info Bool` displays the data declaration for `Bool`.

Data constructors are the actual values.

- Int
- Integer (arbitrarily large)
- Word (zero and up)
- Float
- Double
- Rational
- Fixed
- Scientific (must be [installed](https://hackage.haskell.org/package/scientific))



`Num` is a **type class** (not a type).

```ghci
Prelude> :type (/)
(/) :: Fractional a => a -> a -> a
```

`Fractional a =>` denotes a type class constraint. That is, the type variable “a” must implement the Fractional type class.

`!=` does not exist in Haskell. Use `/=` instead.

Haskell has *if expressions* (not if statements).

Tuple constructor is `(,)`. `:info (,)`. A *product type*, not a *sum type*.

A product type represents a logical conjunction: you must supply both arguments to construct a value.

For tuples, the arity is set in the type and immutable (contrary to lists).

```ghci
Prelude> langs = ["scheme", "haskell"]
Prelude> :type langs langs :: [[Char]] 
```

`langs` is a list of lists, because `[]` is a list, and `[Char]` is a list or chars. `String` is an alias for `[Char]`.

```
Prelude> :type length
length :: Foldable t => t a -> Int
```

Consider `Foldable t` to mean “a list of some type t” for now.

Tuples have zero (called *unit*), two,  three, etc. elements. A tuple with one element does not exist.

A *type class* is a set of operations defined with regard to a polymorphic type.

> Data declarations define new datatypes in Haskell. Data declarations always create a new type constructor, but may or may not create new data constructors. Data declarations are how we refer to the entire definition that begins with the `data` keyword.



So, *data declarations* create types, not data/values.

`type` creates a type alias, and `data` creates a type.

`isEqual :: Eq a => a -> a -> Bool` means that `isEqual` takes *any* type *a* as long is that type has an instance of `Eq` type class.



## 05 Types

A datatype declaration defines a type constructor and data constructors. Data constructors are the values of a particular type; they are also functions that let us create data, or values, of a particular type,

> Type systems in logic and mathematics have been designed to **impose constraints that enforce correctness**.



`->` is the type constructor for functions. It takes arguments and has no data constructors.

`(,)` is the type constructor for the two-tuple.

`[]` is the type constructor for lists.

Unlike the tuple constructor, the function type has no data constructors. The value that shows up at term level is the function. Functions are values.







## The End











