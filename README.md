`apply-refact` applies refactorings specified by the
[`refact`](https://hackage.haskell.org/package/refact) package. It is currently
integrated into `hlint` to enable the automatic application of suggestions.

# install

```shell
stack --resolver=nightly install apply-refact
```

executable name is `refactor`

#### Hlint Integration example

```shell
hlint src/Main.hs --refactor --refactor-options="--inplace"
```

# Example Usage

<img src="http://i.imgur.com/7YXoVft.gif">

```
# test.hs

foo = (x)

# hlint.refact -- produced by hlint --serialise
[("test.hs:1:7: Warning: Redundant bracket\nFound:\n  (x)\nWhy not:\n
x\n",[Replace {rtype = Expr, pos = SrcSpan {startLine = 1, startCol = 7, endLine
= 1, endCol = 10}, subts = [("x",SrcSpan {startLine = 1, startCol = 8, endLine =
1, endCol = 9})], orig = "x"}])]

> refactor test.hs --refact-file hlint.refact
foo = x
```

One of either the input file or `--refact-file` must be specified on the command
line. If an input file is specified but not `--refact-file` then `refactor` will
accept refactorings from stdin and vice versa.

The `-i` option can be specified to perform the refactoring inplace overwriting
the input file. This option is ignored when input is read from stdin.

The `-s` option can be specified to perform a stepwise evaluation of the refact
file. The user is prompted whether to perform each hint before it is performed.

The `--pos` option is intended to be used by tooling in order to specify which
specific hint should be performed.

Multiple `-X` options may be provided to specify additional default language pragmas which might affect parsing, such as `-XLambdaCase` or `-XRankNTypes`.

## Refact Files

Refact files should be the result of `show` on a value of type `[(String,
[Refactoring SrcSpan])]`. The string is a description of the refactoring, one
description can have many associated refactoring steps.


## Library Structure

The executable is provide so that libraries can use `apply-refact` without depending on the package.
The implementation relies on `ghc-exactprint` which depends itself on GHC. A
transitive dependancy that most developers wish to avoid!


# Reporting Bugs

If the program produces a syntactically incorrect result then this is a bug.
Please open an issue on the issue tracker with precise instructions about how to
reproduce it.

1. The input file
2. The refact file
3. The command used to invoke `refactor`

There are some known problems with CPP processing. If your library contains CPP
directives other than `#ifdef` it is quite likely that the result will be
unexpected.

# Debugging

There are also two hidden flags which can be useful for debugging.

#### `--debug`

Outputs the GHC AST.
