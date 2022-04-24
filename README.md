# hs-monkey-interpreter

An interpreter for the [Monkey programming language](https://monkeylang.org/)
written in Haskell.

![The official monkey logo](assets/images/monkey-logo.png)

## Usage

### Start the REPL

```
stack run
```

### Run the tests

All tests

```
stack test
```

A specific test

```
stack test --ta '-m <pattern>'
```

For e.g. test the lexer or parser

```
stack test --ta '-m lexer'
stack test --ta '-m parser'
```
