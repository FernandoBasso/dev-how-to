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

## Array Subscript asterisk * and at @

`man bash` ([man bash on arrays](https://www.gnu.org/software/bash/manual/bash.html#Arrays)):

> Any element of an array may be referenced using `${name[subscript]}`. If the subscript is `@` or `*`, the word expands to all members of the array name. These subscripts differ only when the word appears within double quotes. If the word is double-quoted, `${name[*]}` expands to a single word with the value of each array member separated by the first character of the IFS variable, and `${name[@]}` expands each element of name to a separate word.

By default the value of `IFS` is a single space. Let's apply this to convert an array to a string.

First, create an array with four letters, then, to see the difference between `[*]` and `[@]`, let's use `printf '%s\n'`, since the specifier `%s` is reused as necessary to consume all given parameters:

```shell-session
$ arr=(a b c d)

# <1>
$ printf '“%s”\n' "${arr[*]}"
“a b c d”

# <2>
$ printf '“%s”\n' "${arr[@]}"
“a”
“b”
“c”
“d”
```

<1> The array is expanded to a single element, thus `“%s”\n` is used only once, and the entire output is a single line inside the curly double quotes.

<2> Each element of the array is expanded to a separate word, thus causing `“%s”\n` to be used four times, producing the four output lines. With `[*]` we can convert an array into a string.



## Convert Array To String

We can get an array and turn it into a string with the elements separated by spaces. From
Create an array of numbers:

```shell-session
$ nums=(1 2 3 4)
```

Get the length of the array:

```shell-session
$ echo "${#nums}"
4
```

Use the `[*]` operator to turn the array into a string:

```shell-session
$ strnums="${nums[*]}"
```

The resulting value now has length 7, because our string consists of four digits and three spaces.
```shell-session
$ echo "${#strnums}"
7
```

Show that it loops only once, printing the entire string in a single run:
```shell-session
$ for x in "${strnums[@]}" ; do echo "“$x”" ; done
“1 2 3 4”
```

Now that `strnums` is  a string, even using `[*]` to loop produces a single object:

```shell-session
$ for x in "${strnums[*]}" ; do echo "“$x”" ; done
“1 2 3 4”
```





## Links and Resources

- [Bash Manual: Arrays](https://www.gnu.org/software/bash/manual/bash.html#Arrays)
- [Bash Manual: Special Parameters](https://www.gnu.org/software/bash/manual/bash.html#Special-Parameters)
- [Bash Manual: Word Splitting](https://www.gnu.org/software/bash/manual/bash.html#Word-Splitting)

