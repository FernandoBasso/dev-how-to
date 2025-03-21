# LeetCode 30 Days of JavaScript

## Introduction

- [30 Days of JavaScript study plan on LeetCode](https://leetcode.com/studyplan/30-days-of-javascript/).

Check the full source code, **including unit tests** in the [Gitlab repository for this project](https://gitlab.com/devhowto/dev-how-to/-/tree/main/src/javascript/leetcode30day/src).

## Performance

Sometimes using some new ECMAScript features will result in less performant results (both in terms of execution time and memory footprint).

For example, using [...spread is orders of magnitude more computationally costly](../performance.md) than good old `Array.prototype.push()` to add elements an array, or copy an array.

Similarly, recursion, call stack and the like are more costly (in JavaScript) than some good old for loop approaches.

Using helper functions, immutable data structures and the best coding practices regarding readability and elegance don't always result in the most performant code.

That is why some of the problems are implemented in a few different ways.
Some approach to make it more performant, and some other approaches to use a more functional programming style, immutable data structures, or some other thing we might find fun and/or useful to explore.

## LeetCode comment snippet

Snippet to paste on top of every solution:

```text
//
// More explanations, examples and solutions on my Gitlab repo:
//
// https://gitlab.com/fernandobasso/dev-how-to/-/tree/devel/javascript/leetcode30day
//
```
