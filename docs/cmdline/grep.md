# grep - Global Regular Expression and Print


## Intro

- https://www.gnu.org/software/grep/


## Search in files with given extensions

```bash
$ grep --color -nrH --null \
    --include '*.md' --include '*.hs' \
    -e 'case.*of' ../../
```

## Exclude certain directories

For example, search for “default-config”, except in files inside the `node_modules` and `dist` directories:

```bash
$ grep --exclude-dir node_modules,dist -rl default-config
```
