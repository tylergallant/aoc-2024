module Day01 where

import Data.Char
import Data.Bifunctor
import Data.List
import Data.Maybe
import Paths_aoc2024
import Text.ParserCombinators.ReadP

type Input = ([Int], [Int])
type Output = Int

type Solution = Input -> Output

parser :: ReadP Input
parser = bimap sort sort . unzip <$> manyTill line eof
  where
    line = (,) <$> num <*> num
    num = fmap read $ munch1 isDigit <* skipSpaces

runSolution :: Solution -> String -> Maybe Output
runSolution solution = processResult . parseAndSolve
  where
    fullParses = null . snd
    parseAndSolve = readP_to_S $ solution <$> parser
    processResult = listToMaybe . fmap fst . filter fullParses

solution1 :: Solution
solution1 input = sum distances
  where
    pairs = uncurry zip input
    distances = abs . uncurry (-) <$> pairs

solution2 :: Solution
solution2 (listOne, listTwo) = score listOne listTwo
  where
    score [] _ = 0
    score (x:xs) ys = x * countAppearances x ys + score xs ys
    countAppearances x = length . elemIndices x

day01 :: IO ()
day01 = do
  inputFileName <- getDataFileName "day01-input.txt"
  input <- readFile inputFileName
  print $ runSolution solution1 input
  print $ runSolution solution2 input
