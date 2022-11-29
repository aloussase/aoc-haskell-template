module Main where

import           Gen     (generateComponentForDay)
import           Options


main :: IO ()
main = parseOptions >>= \opts ->
  case opts of
    Generate genOpts -> generateComponentForDay (optDayNumber genOpts)
