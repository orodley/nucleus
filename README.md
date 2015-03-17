A simple, self-hosting Lisp compiler, targeting LLVM.

Building
========

First, build the nucleus compiler with the bootstrap compiler:

```
$ ./boot.sh
```

Then, compile the compiler with itself:

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
