---
*description*: Notes from the book Haskell From First Principles
---

# Haskell From First Principles (notes)



## 1 All You Need is Lambda λ



Lambda Calculus, a model of computation devised in the 1930s by Alonzo Church. A calculus is a method of calculation or reasoning; the lambda calculus is one process for formalizing a method. Like Turing machines, the lambda calculus formalizes the concept of effective computability, thus determining which problems, or classes of problems can be solved.

Some languages in this general category incorporate features into the language that aren’t translatable into lambda expressions. Haskell is a pure functional language because it does not.

Some functional programming languages incorporate features that aren't translatable into lambda expressions, there fore, they are not *pure*. Haskell is a *pure* functional programming language because it does not incorporate features that can't be translated into λ-expressions.

purity == referential transparency

Referential transparency means that the same function, given the same values to evaluate, will always return the same result in pure functional programming, as they do in math.

function is a mapping input(s) to output.

input set is the domain

set of possible outputs is called the codomain

Lambda calculus has tree three basic components, or *lambda terms*: expressions, variables, and abstractions.



## 2 Hello Haskell

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

## div, quot, rem, mod, divMod, quotRem



https://stackoverflow.com/questions/8111120/integral-operators-quot-vs-div

https://wiki.haskell.org/Let_vs._Where



## Chapter 3

```ghci
Prelude> :type "Cati"
"Cati" :: [Char]
```

`String` is a type alias (or synonym) for a list of `Char`.





## The End











