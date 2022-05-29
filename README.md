# How To

- [How To](#how-to)
  - [Intro](#intro)
    - [Examples](#examples)
  - [Why‽](#why)
  - [Ideas](#ideas)

## Intro

My personal repository of tips, tutorials, ideas, code, solutions and whatever else I see fit to put in here 😅.

The idea is to collect everything I research and study in a single place (the exception to this is my math studies which I store somewhere else) so that I can open an editor or do searches from a single place, instead of trying to remember in which directory I did something.



## Commit Messages

As of May 2022, I'm starting to use [Conventional Commits](https://www.conventionalcommits.org), always making sure to use a proper commit *type*  and a *scope* (when a *scope* makes sense):

For programming languages, I use the programming language extension as the commit *type*:

- `c` for the C language;
- `rb` for Ruby;
- `js` for JavaScript;
- `ts` for TypeScript;
- `hs` for Haskell;
- `purs` for PureScript;
- `scm` for Scheme;
- `rkt` for Racket Scheme;
- ...and so on and so forth...



For libraries or frameworks I may use a mix of things, or reasonable abbreviations:

- `cmdline`: general shell-related stuff;
- `vim`: guess!
- `emacs`: guess!
- `xfce`: Xfce Linux Window Manager;
- `thunar`: Xfce file manager;



For books or special “projects” I would have their initials or something minimally identifiable:

- `hffp`: commits related to the book Haskell From First Principles;
- `ckr`: for C K&R book;
- `codewars`: Codewars challenges;
- `hackerrank`: Hakcerrank challenges;



Examples of commits:

```text
ts(ts50): Add l43 promisify example

hffp: Add solution for chapter 10 exercises

vim(fzf): Add fzf text search key binding
```



### Git Setup

Make sure you properly set the githooks path:

```shell-session
$ gi
```





### Helper Script for Commit Types

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


