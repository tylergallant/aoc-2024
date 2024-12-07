module Day04 where

import Control.Monad
import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP
import Utils

type Coord = (Int, Int)
type Letter = (Coord, Char)
type Grid = [Letter]
type Direction = Grid -> Coord -> String
type Input = Grid
type Output = Int

pLetter :: ReadP (Int -> Int -> Letter)
pLetter = labelLetter <$> get
  where labelLetter c y x = ((x, y), c)

pLine :: ReadP (Int -> Grid)
pLine = fmap labelLine $ manyTill pLetter $ char '\n'
  where
    labelLine letters y = zipWith (labelLetter y) letters [0..]
    labelLetter y letter = letter y

parser :: ReadP Input
parser = labelLines <$> manyTill pLine eof
  where labelLines line = join $ zipWith ($) line [0..]

direction :: (Int -> Int) -> (Int -> Int) -> Direction
direction dx dy grid (x, y) = fromMaybe "" $ do
  c <- lookup (x, y) grid
  return $ c : direction dx dy grid next
  where next = (dx x, dy y)

-- the "drop 1" here is to get rid of the (id, id) case
directions :: [Direction]
directions = drop 1 $ direction <$> deltas <*> deltas
  where deltas = [id, pred, succ]

strings :: Grid -> Coord -> [String]
strings grid coord = fromCoord <$> directions
  where fromCoord dir = dir grid coord

countXmas :: Grid -> Coord -> Int
countXmas grid = length . filter id . fmap isXmas . strings grid
  where isXmas = isPrefixOf "XMAS"

solution1 :: Solution Input Output
solution1 grid = sum $ xmasCount <$> filter onlyXs grid
  where
    xmasCount = countXmas grid . fst
    onlyXs = (=='X') . snd

day04 :: IO ()
day04 = do
  input <- getInput "day04-input.txt"
  let solve = createSolver parser input
  solve solution1
