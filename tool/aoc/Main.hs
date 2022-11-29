module Main where

import           Aoc.Tool.Gen     (generateComponentForDay)
import           Aoc.Tool.Options


main :: IO ()
main = parseOptions >>= \opts ->
  case opts of
    Generate genOpts -> generateComponentForDay (optDayNumber genOpts)
