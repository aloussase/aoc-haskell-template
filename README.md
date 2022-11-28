# Advent of Code Solutions

## Solutions

Solutions are divided into modules in the following manner:

- Each day has its separate module
- Each part of the day also has its separate module, nested in the corresponding day's
- There may be a `Common.hs` module inside a day to factor out common functionality in
  parts I and II.

Each solution should have its corresponding test, which is usually the sample input of the
problem.

## Puzzle inputs

The puzzle inputs are located in the `puzzle-inputs` directory. These must be named following the
convention: "day{day number}.txt".

## Running

There is a cabal project containing a program to run a given solution. It can be invoked with
`cabal run` and passed the number of day as an argument.

## License

MIT
