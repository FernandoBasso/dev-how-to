---
description: head command line examples and tips
---



@foo @bar

## Read N First Lines

By default, prints first 10 lines of each input file:

```shell-session
$ cd /usr/include/gtk-3.0/gtk/
$ head gtk-window.h gtk-window.c
==> gtkwindow.h <==
/* GTK - The GIMP Toolkit
#... more 9 lines..

==> gtkwidget.h <==
/* GTK - The GIMP Toolkit
... more 9 lines...
```

To read first N lines, use `-n` or `--lines`.

```shell-session
head --lines=25 file.txt
```



## Read First N Chars

Use the `-c`, or `--bytes` options:

```shell-session
head --bytes=128
```



