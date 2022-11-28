module DayOneSpec where

import           Test.Hspec

import           Aoc.Day.One.PartTwo

main :: IO ()
main = hspec spec

spec :: Spec
spec = partOneSpec

partOneSpec :: Spec
partOneSpec = do
  describe "Part one" $ do
    it "Returns 2 when given 14" $
      solution "14" `shouldReturn` 2

    it "Returns 966 when given 1969" $
      solution "1969" `shouldReturn` 966

    it "Returns 50346 when given 100756" $
      solution "100756" `shouldReturn` 50346
