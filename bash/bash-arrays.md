# Bash Arrays

[TOC]

## Introduction to Bash Arrays

### Create And Use An Array

Create an array of numbers. Use `()`, and, unlike in many other languages, separate elements with spaces, not commas.

```shell-session
$ nums=(1 2 3 4)
```

Use the elements of the array in a loop.

```shell-session
$ for n in "${nums[@]}" ; do printf '    %s\n' "Iteration $n" ; done
    Iteration 1
    Iteration 2
    Iteration 3
    Iteration 4
```

Get the length of the array.

```shell-session
$ echo "${#nums[@]}"
4
```

Append elements to the array:

```shell-session
$ nums+=(5 6)

$ echo "${#nums[@]}"
6

$ echo "${nums[@]}"
1 2 3 4 5 6
```





## Links and Resources

- [Bash Manual: Arrays](https://www.gnu.org/software/bash/manual/bash.html#Arrays)
- [Bash Manual: Special Parameters](https://www.gnu.org/software/bash/manual/bash.html#Special-Parameters)
- [Bash Manual: Word Splitting](https://www.gnu.org/software/bash/manual/bash.html#Word-Splitting)

