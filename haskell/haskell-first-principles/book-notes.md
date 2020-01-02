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

Stack installation, then add this to `~/.stack/global-project/stack.yaml`:

```yaml
resolver: lts-12.10
packages: []
```



```
$ stack ghci

:set prompt "λ "

λ :quit
λ :q

λ :set prompt "λ "

λ 
```

