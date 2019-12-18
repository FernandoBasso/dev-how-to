# HackerRank Shell
[TOC]

## Intro
Unless otherwise noted, assume all scripts contain the following shebang:

```
#!/usr/bin/env bash
```



## Easy Challenges

https://www.hackerrank.com/domains/shell



### Let's Echo
Tags: #cmdline #shell #bash #echo #printf
Links: [challenge](https://www.hackerrank.com/challenges/bash-tutorials-lets-echo)

```shell-session
$ echo HELLO

$ printf '%s\n' HELLO
```



### Looping With Numbers

* Tags: #cmdline #shell #bash #numbers #looping #for
* Links: [challenge](https://www.hackerrank.com/challenges/bash-tutorials---looping-with-numbers)

```shell
for (( i = 1; i <= 9; ++i ))
do
  echo "$i"
done
```


Or using ranges:

```shell-session
$ printf '%d\n' {1..50}
```



### Looping And Skipping

* Tags: #cmdline, #numbers #looping #for
* Links: [challenge](https://www.hackerrank.com/challenges/bash-tutorials---looping-and-skipping)

```shell
for (( i = 1; i <= 9; ++i ))
do
  if (( i % 2 == 0 ))
  then
    continue
  fi
  echo "$i"
done
```
```shell-session
$ bash script.sh
1
3
5
7
9
```


Could also use ''echo:''
```shell-session
$ echo -ne {1..9..2} '\n'
```


The `-e` option is to enable some escapes. `help echo` for more.

Or using `seq`:
```shell-session
$ seq -s ' ' 1 2 9
```


### A Personalized Echo
* Tags: #cmdline #read #echo
* Links: [challenge](https://www.hackerrank.com/challenges/bash-tutorials---a-personalized-echo)

```shell-session
$ read -r name
$ printf 'Welcome %s\n' "$name"
```



### The World of Numbers

* Tags: #cmdline #shell #bash #numbers #math #bc #ranges
* Links: [challenge](https://www.hackerrank.com/challenges/bash-tutorials---the-world-of-numbers)

First, see this clever use of range to produce the math expressions:

```shell-session
$ read -r x y
8 2

$ printf '%s\n' "$x"{+,-,*,/}"$y"
8+2
8-2
8*2
8/2
```

Then, feed those expressions to `bc`:

```shell-session
$ read -r x y
8 2

$ printf '%s\n' "scale=2; $x"{+,-,*,/}"$y" | bc
10
6
16
4.00
```

If `y` is _negative_, like `-2` we would receive an error:

```shell-session
$ read -r x y
5 -2

$ printf '%s\n' "scale=2; $x"{+,-,*,/}"$y" | bc
3
(standard_in) 2: syntax error
-10
-2.50
```

Adding parenthesis prevents the error, because our expression would be like `5--2`, but `5-(-2)` is OK with `bc`:

```shel-session
$ read -r x y
5 -2

$ printf '%s\n' "scale=2; $x"{+,-,*,/}"($y)" | bc
3
7
-10
-2.50
```

Or something more manual and verbose:

```shell
read x </dev/stdin
read y </dev/stdin

printf '%d\n' $(( x + y ))
printf '%d\n' $(( x - y ))
printf '%d\n' $(( x * y ))
printf '%d\n' $(( x / y ))
```

NOTE: The challenge wants integer division, so, we simply omit `bc`'s scale special variable.



```shell
read -r answer

case "$answer" in
  [Yy]*)
    printf '%s\n' YES
    ;;
  [Nn]*)
    printf '%s\n' NO
    ;;
  *)
    printf '%s\n' 'What the poop‽ 💩'
    ;;
esac
```

```shell-session
$ bash script.sh 
yes
YES

$ bash script.sh 
Y
YES

$ bash script.sh
n
NO

$ bash script.sh
lol
What the poop‽ 💩
```



### Getting started with conditionals
* Tags: #cmdline #shell #bash #conditionals
* Links: [challenge](https://www.hackerrank.com/challenges/bash-tutorials---getting-started-with-conditionals)

```bash
read -r answer

case "$answer" in
  [Yy]*)
    printf '%s\n' YES
    ;;
  [Nn]*)
    printf '%s\n' NO
    ;;
  *)
    printf '%s\n' 'What the poop‽ 💩'
    ;;
esac
```

```shell-session
$ bash script.sh 
yes
YES

$ bash script.sh 
Y
YES

$ bash script.sh
n
NO

$ bash script.sh
lol
What the poop‽ 💩
```



### More on Conditionals

* Tags: #cmdline #shell #bash #conditionals #math
* Links: [challenge](https://www.hackerrank.com/challenges/bash-tutorials---more-on-conditionals)

Solution based on side lengths.

* equilateral: x == y && y == z
* scalene: x != y && y != z && z != x
* isosceles: any other

```bash
read -r x
read -r y
read -r z

[[ "$x" == "$y" ]] && [[ "$y" == "$z" ]] && echo EQUILATERAL && exit 0
[[ "$x" != "$y" ]] && [[ "$y" != "$z" ]] && [[ "$z" != "$x" ]] && echo SCALENE && exit 0
echo ISOSCELES && exit 0
```



### Arithmetic Operations

* Tags: #cmdline #shell #bash #math #bc
* Links: [challenge](https://www.hackerrank.com/challenges/bash-tutorials---arithmetic-operations)

```bash
expression="$1"
printf '%.3f\n' "$(echo "$expression" | bc -l)"
```

`bc -l` produces up to 6 decimal places. If we use `bc` scale to 3, for instance, depending on the result, we would produce wrong results because `printf %f` format specifier does rounding by itself.

`bc` scale is 0 by default if not explicitly set. Also, `bc` does no rounding.

`printf` rounds up from 6, and down from 5:

```shell-session
$ printf '%.3f\n' 1.2583
1.258
$ printf '%.3f\n' 1.2585
1.258
$ printf '%.3f\n' 1.2586
1.259
```

Only when the number after 8 passes 5, that is, 6 and above, is that the number is rounded up to 1.259. If one uses `scale=3` in `bc`, then it truncates (does not round) to three decimal places and `printf` has no way to round up, making the solution to the exercise incorrect. Therefore, we use `bc -l` without scale, or use `scale=4` at least.



### Compute the Average

* Tags: #cmdline #shell #bash #math
* Links: [challenge](https://www.hackerrank.com/challenges/bash-tutorials---compute-the-average)

```bash
read -r n
sum=0

if [[ "$n" == 0 ]]
then
  printf '%.3f\n' "$(echo 'scale=4; 0' | bc -l)"
  exit 0
fi

for ((i = 0; i < n; ++i))
do
  read -r x
  sum=$((sum + x))
done

printf '%.3f\n' "$(echo "scale=4; $sum / $n" | bc -l)"
```

We used `scale=4` by the same reasons described earlier about truncating and rounding.



### cut Challenges

* Tags: #cmdline #shell #bash #cut

```shell-session
$ cut -b 3 -

$ cut -b 2,7 -

$ cut -b 2-7 -

$ cut -b 1-4 -

$ cut -d $'\t' -f 1,2,3 -

$ cut -c 13- -

$ cut -d ' ' -f 4 -

$ cut -d ' ' -f 1,2,3 -

$ cut -d $'\t' -f 2- -
```



### Head of Text File Challenges	

```shell-session
$ head -n 20

$ head -c 20
```



### Middle of a Text File

* Tags: #cmdline #shell #bash #sed
* Links: [challenge](https://www.hackerrank.com/challenges/text-processing-in-linux---the-middle-of-a-text-file)

```shell-session
$ sed -n '12,22 p'
```



### Tail of a Text File 1 and 2

* Tags: #cmdline #shell #bash #tail
* Links: [challenge](https://www.hackerrank.com/challenges/text-processing-tail-1)

```shell-session
$ tail -n 20 -

$ tail -c 20 -
```



### tr Command 1

* Tags: #cmdline #shell #bash #tr #here-document #assignment
* Links: [challenge](https://www.hackerrank.com/challenges/text-processing-tr-1)

```shell-session
# Assign some text to the variable `input'.
$ read -r -d '' input << 'EOF'
int i = (int) 5.8;
int res = (23 + i) * 2;
EOF

# Inspect `input' contents.
$ echo "$input"
int i = (int) 5.8;
int res = (23 + i) * 2;

# Apply `tr' to `input' and see ( and ) replaced with [ and ].
$ echo "$input" | tr '()' '[]'
int i = [int] 5.8;
int res = [23 + i] * 2;
```

A [Here Document](https://www.gnu.org/software/bash/manual/bash.html#Here-Documents) is used to assign lines of text to the variable `input`.



### tr Command 2

* Tags: #cmdline #shell #bash #tr
* Links: [challenge](https://www.hackerrank.com/challenges/text-processing-tr-2)

```shell-session
$ tr -d 'a-z'
```



### tr Command 3

* Tags: #cmdline #shell #bash #tr
* Links: [challenge](https://www.hackerrank.com/challenges/text-processing-tr-3)

```shell-session
$ tr -s ' '
```



### sort Lines Challenges

* Tags: #cmdline #shell #bash #sort
* Links: [challenge](https://www.hackerrank.com/challenges/text-processing-sort-1)

```shell-session
$ echo -e 'aa\nbb\naa\ncc\nff\ncc' | sort -
aa
aa
bb
cc
cc
ff

$ echo -e 'aa\nbb\naa\ncc\nff\ncc' | sort -r -
ff
cc
cc
bb
aa
aa

$ echo -e '2.1\n3\n0.2\n0' | sort -n -
0
0.2
2.1
3

$ echo -e '2.1\n3\n0.2\n0' | sort -nr -
3
2.1
0.2
0

# Sort by field 2, taking Tab as field separator.
$ sort -t $'\t' -nr -k 2 -

# Same, but in ascending order.
$ sort -t $'\t' -n -k 2 -

# This time the delimiter is a “|” character
$ sort -t '|' -nr -k 2 -
```



### uniq Challenges

* Tags: #cmdline #shell #bash #uniq
* Links: [challenge](https://www.hackerrank.com/challenges/text-processing-in-linux-the-uniq-command-1)

```shell-session
$ uniq -
​```

```

Display the count of lines that were uniqfied and the uniqfied lines without leading whitespace/tabs:

```shell-session
$ read -r -d '' lines << 'EOF'
> foo
> foo
> bar
> bar
> bar
> tux
> EOF

$ echo "$lines" | uniq -c | sed 's/ \+\([0-9]\+ [^ ]\+\)/\1/'
2 foo
3 bar
1 tux

$ echo "$lines" | uniq -c | sed 's/^[[:space:]]*//g'
2 foo
3 bar
1 tux

$ echo "$lines" | uniq -c | cut -b 7- -
2 foo
3 bar
1 tux

$ echo "$lines" | uniq -c | xargs -l
2 foo
3 bar
1 tux

$ echo "$lines" | uniq -c | xargs -L 1
2 foo
3 bar
1 tux

$ echo "$lines" | uniq -c | colrm 1 6
2 foo
3 bar
1 tux
```












## The End

