== Looping and Skipping

```bash
for num in {1..99} ; do # <1>
    if (( $num % 2 )) ; then # <2>
        printf '%d\n' $num
    fi
done
```

E#1:

- `echo $(( 4 % 2 ))` produces 0.
- `echo $(( 3 % 2 ))` produces 1.
- 0 is truthy, 1 is falsy.

https://unix.stackexchange.com/questions/110348/how-do-i-get-the-list-of-exit-codes-and-or-return-codes-and-meaning-for-a-comm

In C/bash and other langs, 0 is truthy because all other positive or negative integer can then be used to represent a type of error, something similar to HTTP status codes. Try `man ping` and search for `code 1` and `code 2` or `man wget` and search for `EXIT STATUS`.


E#2: Could also use `$(seq 99)` instead of the range. It would be using another
  process and an external program, but it would work.

Also,

```bash
seq 1 2 99
```

would start at 1, increment by 2, and stop on 99. With a range, this would do the same.

```bash
{1..99..2}
```

And there is some more trickery that could be done. Note the space between `'\n'` and `{` in the first example and its output compared to the second example.

```
$ echo -e '\n' {1..9..2}

 1 3 5 7 9

$ echo -e '\n'{1..9..2}

1
3
5
7
9

```

Or using `printf` instead:

```
$ printf '%d\n' {1..9..2}
1
3
5
7
9
```
