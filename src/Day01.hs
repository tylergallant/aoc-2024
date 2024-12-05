module Day01 where

import Data.Char
import Data.Bifunctor
import Data.List
import Paths_aoc2024
import Text.ParserCombinators.ReadP
import Utils

type Input = ([Int], [Int])
type Output = Int

parser :: ReadP Input
parser = bimap sort sort . unzip <$> manyTill line eof
  where
    line = (,) <$> num <*> num
    num = fmap read $ munch1 isDigit <* skipSpaces

solution1 :: Solution Input Output
solution1 input = sum distances
  where
    pairs = uncurry zip input
    distances = abs . uncurry (-) <$> pairs

solution2 :: Solution Input Output
solution2 (listOne, listTwo) = score listOne listTwo
  where
    score [] _ = 0
    score (x:xs) ys = x * countAppearances x ys + score xs ys
    countAppearances x = length . elemIndices x

day01 :: IO ()
day01 = do
  inputFileName <- getDataFileName "day01-input.txt"
  input <- readFile inputFileName
  let solve = createSolver parser input
  solve solution1
  solve solution2
