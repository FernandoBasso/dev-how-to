# Inspecting The Repository

- [useful log commands](#useful-log-commands)
- [useful branch commands](#useful-branch-commands)


## useful log commands

Logging the last five commits:

```shell
git log -5
git log -n 5
git log -n 5 --oneline
```

Show stat, diff, or both:
```shell
git log --stat
git log --patch
git log --patch-with-stat
```

List all branches containing a given commit:

## useful branch commands

```shell
git branch --contains <commit>

# also include remotes
git branch --remotes --contains <commit>
```

