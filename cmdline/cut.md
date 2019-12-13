---
description: cut command line examples and tips
---



intro
-----
Read man cut, info cut and cut --help.

Select specific chars from each input line:

```shell-session
$ cut --bytes=1,3,5,9,13 <<<'May The Force'
MyTFe

$ cut --characters=1,3,5 <<<$'May The\nForce Be With'
MyT
Fre
```


For a range of chars, use the “x-y” notation:

```shell-session
$ cut --characters=1-5 <<<'hello-world'
hello
```

