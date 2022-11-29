module Gen (generateComponentForDay, Day) where

import           Control.Monad    (unless)

import           Data.List
import           Data.Maybe       (fromJust)
import           Print            (pp, ppErr)
import           System.Directory
import           System.Exit      (exitFailure, exitSuccess)
import           System.FilePath

type Day = Int

generateComponentForDay :: Day -> IO ()
generateComponentForDay day = do
  isCabalProject <- checkCurrentDirIsCabalProject

  unless isCabalProject $ do
    ppErr "Not in a cabal project"
    exitFailure

  createComponentForDay day
  pp "\nFinished creating component.\n"

  let dir = fromJust $ directoryForComponent day
  pp "Copy this in your cabal file to add the new component to the build:\n"
  pp $ formatForCabalFile $ componentFileNames dir
  pp "\n"

-- | 'createDirectoriesForComponent' creates the necessary directories for the new component.
createComponentForDay :: Day -> IO ()
createComponentForDay day = maybe invalidDay createComponent $ directoryForComponent day
  where
    createComponent dirName = mapM_ createComponentFile $ componentFileNames dirName
    invalidDay = ppErr ("Invalid number for component day: " <> show day) >> exitFailure

-- | 'componentFileNames' returns an array of the component's file names given the component directory.
componentFileNames :: FilePath -> [FilePath]
componentFileNames dirName = map (dirName </>) ["PartOne.hs", "PartTwo.hs", "Common.hs"]

createComponentFile :: FilePath -> IO ()
createComponentFile filepath = do
  fileExists <- doesFileExist filepath
  if fileExists then do
    pp $ "Component file already exists: " <> filepath
    exitSuccess
  else do
    pp $ "Creating component file: " <> filepath
    createDirectoryIfMissing True $ takeDirectory filepath
    writeFile filepath $ createModuleContents (toModuleName filepath)


createModuleContents :: String -> String
createModuleContents moduleName = mconcat
  [ "module " <> moduleName <> " where", "\n\n"
  , if "Common" `isInfixOf` moduleName then "" else "solution :: IO a\nsolution = undefined"
  ]

-- | 'checkCurrentDirIsCabalProject' checks that the current directory is a Cabal project.
checkCurrentDirIsCabalProject :: IO Bool
checkCurrentDirIsCabalProject = any ((== ".cabal") . takeExtension) <$> listDirectory  "."

directoryForComponent :: Day -> Maybe FilePath
directoryForComponent day = (\name -> "lib" </> "Aoc" </> "Day" </> name) <$> toDirName day

-- | 'formatForCabalFile' formats the list of component paths to a shape suitable to paste in
-- the "other-modules" section of the cabal file.
formatForCabalFile :: [FilePath] -> String
formatForCabalFile [x]    = "   " <> toModuleName x
formatForCabalFile (x:xs) = "   " <> toModuleName x <> ",\n" <> formatForCabalFile xs
formatForCabalFile []     = ""

toModuleName :: FilePath -> String
toModuleName =
    drop (length ("lib." :: String))
  . dropExtension
  . map (\c -> if c == '/' then '.' else c)

toDirName :: Day -> Maybe FilePath
toDirName 1   = pure "One"
toDirName 2   = pure "Two"
toDirName 3   = pure "Three"
toDirName 4   = pure "Four"
toDirName 5   = pure "Five"
toDirName 6   = pure "Six"
toDirName 7   = pure "Seven"
toDirName 8   = pure "Eight"
toDirName 9   = pure "Nine"
toDirName 10  = pure "Ten"
toDirName 11  = pure "Eleven"
toDirName 12  = pure "Twelve"
toDirName 13  = pure "Thirteen"
toDirName 14  = pure "Fourteen"
toDirName 15  = pure "Fifteen"
toDirName 16  = pure "Sixteen"
toDirName 17  = pure "Seventeen"
toDirName 18  = pure "Eighteen"
toDirName 19  = pure "Nineteen"
toDirName 20  = pure "Twenty"
toDirName 21  = pure "TwentyOne"
toDirName 22  = pure "TwentyTwo"
toDirName 23  = pure "TwentyThree"
toDirName 24  = pure "TwentyFour"
toDirName 25  = pure "TwentyFive"
toDirName day = Nothing

