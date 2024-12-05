module Day01 where

import Data.Char
import Data.Bifunctor
import Data.List
import Data.Maybe
import Paths_aoc2024
import Text.ParserCombinators.ReadP

type Input = [(Int, Int)]
type Output = Int

parser :: ReadP [(Int, Int)]
parser = manyTill line eof
  where
    line = (,) <$> num <*> num
    num = fmap read $ munch1 isDigit <* skipSpaces

runParser :: ReadP a -> String -> Maybe a
runParser p = listToMaybe . fmap fst . filter fullParses . readP_to_S p
  where fullParses = null . snd

solution1 :: Input -> Output
solution1 input = sum distances
  where
    pairs = uncurry zip $ bimap sort sort $ unzip input
    distances = abs . uncurry (-) <$> pairs

day01 :: IO ()
day01 = do
  inputFileName <- getDataFileName "day01-input.txt"
  input <- readFile inputFileName
  print $ runParser (solution1 <$> parser) input
