# Haskell Programming From First Principles

My notes and solutions for the book [Haskell From First Principles](https://haskellbook.com/).
Thanks to [Chris](https://bitemyapp.com/) for giving me a PDF copy of the [book](https://haskellbook.com/) for free.
Amazing! ❤️

The [original repo is on Gitlab](https://gitlab.com/programming-studies/haskell-from-first-principles).
The [Github repo](https://github.com/FernandoBasso/Haskell-From-First-Principles) is just a push repo that keeps up to date with what I push to Gitlab.

## ghcup

- https://www.haskell.org/ghcup/

```text
$ curl \
    --proto '=https' \
    --tlsv1.2 \
    -sSf https://get-ghcup.haskell.org \
    | sh

(Press <Enter> once or twice to get the installation going.)

All done!

To start a simple repl, run:
  ghci

To start a new haskell project in the current directory, run:
  cabal init --interactive

To install other GHC versions and tools, run:
  ghcup tui

If you are new to Haskell, check out:

• https://www.haskell.org/ghcup/steps/
```
