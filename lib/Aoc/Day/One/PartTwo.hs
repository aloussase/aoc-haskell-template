module Aoc.Day.One.PartTwo where

import           Aoc.Day.One.Common

import           Data.Text          (Text)


genFuelsForModule :: ModuleMass -> Fuel
genFuelsForModule currentFuel =
  if currentFuel <= 0 then 0
  else currentFuel + genFuelsForModule (calculateFuelForModule currentFuel)

solution :: Text -> IO Int
solution input = do
  let initialFuels = map calculateFuelForModule (parseInput input)
      fuels = map genFuelsForModule initialFuels
  return $ sum fuels
