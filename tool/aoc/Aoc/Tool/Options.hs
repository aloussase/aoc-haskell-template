module Aoc.Tool.Options where

import           Options.Applicative


newtype Command = Generate GenerateOptions deriving Show

newtype GenerateOptions = GenerateOptions
  { optDayNumber :: Int
  }
  deriving Show

generateCommand :: ParserInfo Command
generateCommand = Generate <$> info (GenerateOptions <$> dayNumber)
  (progDesc "Generate an AoC solution component.")
  where
    dayNumber = option auto
      (long "day"
        <> short 'd'
        <> help "The day to generate a solution component for."
        <> metavar "INT")

parseOptions :: IO Command
parseOptions = execParser $ info (opts <**> helper)
  (fullDesc
  <> progDesc "CLI utility make it easier to use Haskell for AoC.")
  where
    opts = hsubparser
      (command "generate" generateCommand)
