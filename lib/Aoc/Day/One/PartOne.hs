module Aoc.Day.One.PartOne
(
  solution
)
where

import           Aoc.Day.One.Common

import           Data.Text          (Text)


solution :: Text -> IO Int
solution input = return $ sum $ map calculateFuelForModule (parseInput input)
