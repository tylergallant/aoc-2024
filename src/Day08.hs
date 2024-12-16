module Day08 where

import Control.Applicative
import Control.Monad.State.Lazy
import Data.Functor
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Text.ParserCombinators.ReadP hiding (get, many)
import Utils

type Pos = (Int, Int)
type Freq = Char
type CityMap = Map Pos (Maybe Freq)
type FreqMap = Map Freq [Pos]
type Parser = StateT Pos ReadP
type Input = CityMap
type Output = Int

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

onMap :: Pos -> Pos -> Bool
onMap (mx, my) (x, y) = x >= 0 && y >= 0 && x <= mx && y <= my

antinode :: Pos -> Pos -> Pos -> Maybe Pos
antinode maxPos p1@(x1, y1) p2@(x2, y2)
  | p1 /= p2 && onMap maxPos p3 = Just p3
  | otherwise = Nothing
  where p3 = (x2 + (x2 - x1), y2 + (y2 - y1))

resonantAntinodes :: Pos -> Pos -> Pos -> [Pos]
resonantAntinodes maxPos p1 p2 = do
  p3 <- maybeToList $ antinode maxPos p1 p2
  p3 : resonantAntinodes maxPos p2 p3

allAntinodes :: Pos -> Pos -> Pos -> [Pos]
allAntinodes maxPos p1 p2 = p1 : p2 : forwardResonants ++ backwardResonants
  where
    forwardResonants = resonantAntinodes maxPos p1 p2
    backwardResonants = resonantAntinodes maxPos p2 p1

freqMap :: CityMap -> FreqMap
freqMap = Map.foldrWithKey' maybeAddAntenna Map.empty
  where
    maybeAddAntenna _ Nothing = id
    maybeAddAntenna pos (Just freq) = Map.alter (addPos pos) freq
    addPos pos Nothing = Just [pos]
    addPos pos (Just ps) = Just $ pos : ps

pairs :: [a] -> [(a, a)]
pairs xs = [(x,y) | (x:ys) <- tails xs, y <- ys]

getMaxPos :: Map Pos a -> Pos
getMaxPos = maybe (0, 0) fst . Map.lookupMax

reduce :: (Foldable t, Eq a) => Map k (t [a]) -> Int
reduce = length . nub . concat . Map.elems . fmap concat

solution1 :: Solution Input Output
solution1 city = reduce $ fmap getAntinodes . pairs <$> freqMap city
  where
    getAntinodes (p1, p2) = catMaybes [getAntinode p1 p2, getAntinode p2 p1]
    getAntinode = antinode maxPos
    maxPos = getMaxPos city

solution2 :: Solution Input Output
solution2 city = reduce $ fmap getAntinodes . pairs <$> freqMap city
  where getAntinodes = uncurry . allAntinodes $ getMaxPos city

day08 :: IO ()
day08 = do
  input <- getInput "day08-input.txt"
  let solve = createSolver parser input
  solve solution1
  solve solution2
