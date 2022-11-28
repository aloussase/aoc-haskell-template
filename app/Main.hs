module Main where

import           Input

import qualified Aoc.Day.One.PartOne as D1P1
import qualified Aoc.Day.One.PartTwo as D1P2

import           Control.Monad       (forM_)
import           System.Environment  (getArgs)


printSolutions :: (Show a, Show b) => Day -> (a, b) -> IO ()
printSolutions day (p1, p2) = do
  putStrLn $ "Solutions for day " <> show (toInt day)
  putStrLn $ "Part I:  " <> show p1
  putStrLn $ "Part II: " <> show p2

solve :: Day -> IO ()
solve day = do
  input <- inputForDay day
  solutions <- case toInt day of
                1 -> (,) <$> D1P1.solution input <*> D1P2.solution input
                _ -> error "Invalid day"
  printSolutions day solutions

main :: IO ()
main = do
  args <- getArgs
  if null args then putStrLn "Missing required argument 'day:int'"
  else forM_ (parseDay $ head args) solve
