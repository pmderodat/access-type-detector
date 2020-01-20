Material for my blog post on AdaCore's blog: <>

Inspired from Martyn Pike's original blog post:
<https://blog.adacore.com/an-expedition-into-libadalang>

Building these programs require recent (as of January 2020) versions of
GNATCOLL and Libadalang:

```sh
gprbuild -Pproj.gpr -p
```

To run the first checker:

```sh
test/ptrfinder1 -Ptest/test.gpr
```

To run the second one:

```sh
test/ptrfinder2 --Ptest/test.gpr --verify
```
