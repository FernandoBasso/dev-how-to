# How To

- [How To](#how-to)
  - [Intro](#intro)
    - [Examples](#examples)
  - [Why‽](#why)
  - [Ideas](#ideas)

## Intro

My personal repository of tips, tutorials, ideas, code, solutions and whatever else I see fit to put in here 😅.

At some point, I started to try to categorize commits. For books or special “projects” I would have their initials or something minimally identifiable:

- hffp: commits related to the book Haskell From First Principles;
- ckr: for C K&R book;
- codewars: Codewars challenges;
- hackerrank: Hakcerrank challenges;

For other stuff, I would try to be reasonable in some way:
- cmdline: general shell-related stuff;
- ruby: Ruby;
- ecmascript: ECMAScript (JavaScript)
- haskell: Haskell stuff;
- node: Node.js;
- vim: guess!
- emacs: guess!
- xfce: Xfce Linux Window Manager;
- thunar: Xfce file manager;

### Examples

hffp: Add ch10 exercises
ghci: configure prompt
xfce: add find cursor shortcut
ruby: add examples of singletons

## Why‽

The idea is to collect everything I research and study in a single place (the exception to this is my math studies which I store somewhere else) so that I can open an editor or do searches from a single place, instead of trying to remember in which directory I did something.

## Commit subject tag

One idea is to create a script that collects all “tags” I have used so far in git commits and try to reuse them consistently instead of making a mess. Hard to avoid the mess, though.

Here's the script:

```
$ git log --format='%s' \
    | grep '^[a-z]\+:' \
    | sed 's/:.\+//' \
    | sort \
    | uniq
add
hffp
repo
wip
```

