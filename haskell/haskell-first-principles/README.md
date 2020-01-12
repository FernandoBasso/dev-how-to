# Haskell From First Principles

https://wiki.haskell.org/IDEs



Using GHCi 8.3.4. Start ghci with this command:

```shell-session
$ stack --resolver lts-12.10 ghci
```

We use this version because it was recommended in the book to avoid too many difference in the output or other surprises.



## Emacs haskell-mode

https://stackoverflow.com/questions/56501112/haskell-emacs-haskell-mode-run-c-h-f-haskell-mode-for-instruction-how-to-setu

I have this in `~/.stack/global-project/stack.yml`:

```yaml
packages: []
resolver: lts-12.10
```

Check `myinit.org` emacs config file in my [dotfiles](https://gitlab.com/fernandobasso/dotfiles/tree/master/.emacs.d) repo.



## HIE - Haskell IDE Engine

Install hie-8.4.3 so that we have GHCi 8.4.3 from lts-12.10 as recommended  in the book.

```
$ stack --resolver lts-12.10 ./install.hs hie-8.4.3
$ stack --resolver lts-12.10 ./install.hs data
```



## VSCode

https://www.vacationlabs.com/haskell/environment-setup.html

