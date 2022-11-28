module Aoc.Day.One.Common where

import           Data.Text (Text)
import qualified Data.Text as T

type ModuleMass = Int
type Fuel = Int

parseInput :: Text -> [ModuleMass]
parseInput = map (read . T.unpack) . T.lines

calculateFuelForModule :: ModuleMass -> Fuel
calculateFuelForModule mass = mass `div` 3 - 2
