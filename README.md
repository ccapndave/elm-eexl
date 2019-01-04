# elm-eexl

## Description

elm-eexl (Elm Expression Language) is a simple expression parser and evaluator for Elm that allows expressions to be evaluated at runtime.

## What's it for?

We develop educational software where the student follows different paths through courses depending on how they are getting on.  For example, if they have attained a certain score in an exercise and have some minimum percentage over the whole unit we might send them down path A, or if they need to do some more work on a particular subject we might send them down path B.  This package allows us to evaluate the expressions implementing this logic, which we load at runtime.

## Technicalities

The library uses [elm/parser](https://package.elm-lang.org/packages/elm/parser/latest/) to parse expressions, and an implementation of [Djikstra's shunting-yard algorithm](https://en.wikipedia.org/wiki/Shunting-yard_algorithm) to handle precedence and associativity.  Type checking is done in an ad-hoc and not particularly smart way, but is enough for our requirements at the moment.

## Supported features

### Operators
- `^`
- `*`
- `+`
- `<=`
- `<`
- `>=`
- `>`
- `==`
- `&&`
- `||`

### Types
- booleans (`true`, `false`)
- integers

### Variables
- `x`, `y`, `myVariable`

### Functions
- `myFunction("inputstring")`

## Limitations

- Only int and bool are directly supported
- Variables are always ints
- Functions always take a single string argument and return an int
- Unary operators are not implemented

## Tests

I made an attempt to auto-generate expressions, run them through [run-elm](https://github.com/jfairbank/run-elm) in order to get their results, and then build a test suite.  Unfortunately it turns out that randomly generating expressions ends up with type errors for almost every case which isn't a very good test suite.  However, I am confident that if some heuristics could be employed when building the expressions the amount of compilation fails could be brought down to a level that would make this work.

In the meantime there is a handcoded test suite that covers a few cases.

## Future development

For the moment this library does everything I need, so I won't be making any update unless I find bugs or our requirements change, but I'm very happy to accept pull requests if anyone wants to implement new features.
