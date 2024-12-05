module Day02 where

import Data.Char
import Data.List
import Text.ParserCombinators.ReadP
import Utils

type Level = Int
type Report = [Level]
type Input = [Report]
type Output = Int

parser :: ReadP Input
parser = manyTill report eof
  where
    report = sepBy1 level space <* newline
    level = read <$> munch1 isDigit
    newline = char '\n'
    space = char ' '

isSafe :: Report -> Bool
isSafe = fst . scan
  where
    scan [] = (True, EQ)
    scan [_] = (True, EQ)
    scan (x:y:xs)
      | 1 <= diff && diff <= 3 = (restSafe && ordering /= LT, GT)
      | -3 <= diff && diff <= -1 = (restSafe && ordering /= GT, LT)
      | otherwise = (False, EQ)
      where
        diff = x - y
        (restSafe, ordering) = scan (y:xs)

solution1 :: Solution Input Output
solution1 = length . elemIndices True . fmap isSafe

day02 :: IO ()
day02 = do
  input <- getInput "day02-input.txt"
  let solve = createSolver parser input
  solve solution1
