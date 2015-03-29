A simple, self-hosting Lisp compiler, targeting LLVM.

Building
========

First you need a precompiled snapshot to bootstrap. If you actually want to
try building it let me know and I'll put some up somewhere.

Then you can compile with:

```
$ ./build.sh
```

Tests
=====

To run the test suite:

```
$ ./run-tests.py
```

or

```
$ ./run-tests.py <subset of tests to run>
```
