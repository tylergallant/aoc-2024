module Day04 where

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
  where labelLines line = concat $ zipWith ($) line [0..]

direction :: (Int -> Int) -> (Int -> Int) -> Direction
direction dx dy grid (x, y) = fromMaybe "" $ do
  c <- lookup (x, y) grid
  return $ c : direction dx dy grid next
  where next = (dx x, dy y)

mirror :: Direction -> Direction
mirror dir grid coord = dir (mirrorGrid grid) $ mirrorCoord coord
  where
    mirrorCoord (x, y) = (negate x, negate y)
    mirrorLetter (c, letter) = (mirrorCoord c, letter)
    mirrorGrid = fmap mirrorLetter

directions :: [(Direction, Direction)]
directions = dirPair . uncurry direction <$> deltas
  where
    dirPair dir = (dir, mirror dir)
    deltas = [(succ, succ), (succ, pred), (id, succ), (succ, id)]

-- Directions are setup so that the first 2 pairs are the diagonals
diagonals :: [(Direction, Direction)]
diagonals = take 2 directions

strings :: Grid -> Coord -> [String]
strings grid coord = concatMap fromCoord directions
  where fromCoord (d1, d2) = [d1 grid coord, d2 grid coord]

countXmas :: Grid -> Coord -> Int
countXmas grid = length . filter id . fmap isXmas . strings grid
  where isXmas = isPrefixOf "XMAS"

countMasX :: Grid -> Coord -> Int
countMasX grid = fromEnum . all (`elem` ["MAS", "SAM"]) . diagonalsAt
  where
    diagonalsAt coord = from coord <$> diagonals
    from c (d1, d2) = nextChar d1 grid c ++ "A" ++ nextChar d2 grid c
    nextChar d g = take 1 . drop 1 . d g

solution1 :: Solution Input Output
solution1 grid = sum $ xmasCount <$> filter onlyXs grid
  where
    xmasCount = countXmas grid . fst
    onlyXs = (=='X') . snd

solution2 :: Solution Input Output
solution2 grid = sum $ masXCount <$> filter onlyAs grid
  where
    masXCount = countMasX grid . fst
    onlyAs = (=='A') . snd

day04 :: IO ()
day04 = do
  input <- getInput "day04-input.txt"
  let solve = createSolver parser input
  solve solution1
  solve solution2
