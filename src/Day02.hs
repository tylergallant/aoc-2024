module Day02 where

import Data.Char
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

checkDir :: Ordering -> Report -> Bool
checkDir _ [] = True
checkDir _ [_] = True
checkDir order (x:y:xs) = compare x y == order && withinRange && recur
  where
    diff = abs $ x - y
    withinRange = 1 <= diff && diff <= 3
    recur = checkDir order $ y : xs

checkReport :: Report -> Bool
checkReport report = checkDir LT report || checkDir GT report

dampen :: Int -> Report -> [Report]
dampen 0 xs = [xs]
dampen _ [] = [[]]
dampen n (x:xs) = ((x:) <$> dampen n xs) ++ dampen (n - 1) xs

scanReport :: Report -> Bool
scanReport = any checkReport . dampen 1

solution1 :: Solution Input Output
solution1 = length . filter id . fmap checkReport

solution2 :: Solution Input Output
solution2 = length . filter id . fmap scanReport

day02 :: IO ()
day02 = do
  input <- getInput "day02-input.txt"
  let solve = createSolver parser input
  solve solution1
  solve solution2
