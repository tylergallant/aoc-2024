module Day08 where

import Control.Applicative
import Control.Monad.State.Lazy
import Data.Functor
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.ReadP hiding (get, many)
import Utils

type Pos = (Int, Int)
type Freq = Char
type Antenna = (Pos, Freq)
type CityMap = Map Pos (Maybe Freq)
type FreqMap = Map Freq [Pos]
type Parser = StateT Pos ReadP
type Input = CityMap
type Output = FreqMap

onMap :: Pos -> Pos -> Bool
onMap (mx, my) (x, y) = x >= 0 && y >= 0 && x <= mx && y <= my

antinodes :: Pos -> Pos -> Pos -> [Pos]
antinodes maxPos (x1, y1) (x2, y2) = onlyOnMap [(x3, y3), (x4, y4)]
  where
    diffX = x1 - x2
    diffY = y1 - y2
    x3 = x1 - diffX * 2
    y3 = y1 - diffY * 2
    x4 = x2 + diffX * 2
    y4 = y2 + diffY * 2
    onlyOnMap = filter $ onMap maxPos

liftParser :: ReadP a -> Parser (Pos, a)
liftParser p = do
  a <- lift p
  (x, y) <- get
  put (x + 1, y)
  return ((x, y), a)

pAntenna :: Parser (Pos, Maybe Freq)
pAntenna = fmap Just <$> p
  where p = liftParser $ satisfy (`notElem` ".\n")

pSpace :: Parser (Pos, Maybe Freq)
pSpace = (Nothing <$) <$> p
  where p = liftParser $ satisfy (== '.')

pElement :: Parser (Pos, Maybe Freq)
pElement = pSpace <|> pAntenna

pNewLine :: Parser ()
pNewLine = do
  void . lift $ char '\n'
  (_, y) <- get
  put (0, y + 1)

pLine :: Parser [(Pos, Maybe Freq)]
pLine = many pElement <* pNewLine

pCity :: Parser CityMap
pCity = Map.fromList . concat <$> pLines
  where pLines = many pLine <* lift eof

parser :: ReadP Input
parser = evalStateT pCity (0, 0)

freqMap :: CityMap -> FreqMap
freqMap = Map.foldrWithKey' maybeAddAntenna Map.empty
  where
    maybeAddAntenna _ Nothing = id
    maybeAddAntenna pos (Just freq) = Map.alter (addPos pos) freq
    addPos pos Nothing = Just [pos]
    addPos pos (Just ps) = Just $ pos : ps

solution1 :: Solution Input Int
solution1 city = reduce $ fmap getAntinodes . pairs <$> freqMap city
  where
    pairs xs = [(x,y) | (x:ys) <- tails xs, y <- ys]
    maxPos = maybe (0, 0) fst $ Map.lookupMax city
    getAntinodes = uncurry $ antinodes maxPos
    reduce = length . nub . concat . Map.elems . fmap concat

day08 :: IO ()
day08 = do
  input <- getInput "day08-input.txt"
  let solve = createSolver parser input
  solve solution1
