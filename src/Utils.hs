module Utils where

import Data.Maybe
import Paths_aoc2024
import Text.ParserCombinators.ReadP

type Solution input output = input -> output

runSolution :: ReadP input -> String -> Solution input output -> Maybe output
runSolution parser input solution = processResult $ parseAndSolve input
  where
    parseAndSolve = readP_to_S $ solution <$> parser
    processResult = listToMaybe . fmap fst . filter fullParses
    fullParses = null . snd

getInput :: FilePath -> IO String
getInput filename = do
  inputFileName <- getDataFileName filename
  readFile inputFileName

createSolver :: Show o => ReadP i -> String -> Solution i o -> IO ()
createSolver parser input = foldMap print . runSolution parser input
