# Bash Parameter Expansion

## % and %% match from the end



![@geirha answer on pattern matching](./bash-parameter-expansion.assets/2021-08-09-07-32-28-my-image.png)

`%/*`removes the shortest string matching `/*` from the end.

```shell
$ v='/etc/conf.d/tmux.conf'

$ echo "${v%/*}"
/etc/conf.d
```

It starts from the end and removes everything until it finds the first “/” (first left to right).

Using `%%` instead of `%` makes it greedy, removing as much as it can:

```shell
$ v='src/config/README.md'

$ echo "${v%%/*}"
src
```

It is **greedy**. It stars at the end and until it finds the last “/” in a left-to-right direction. 


