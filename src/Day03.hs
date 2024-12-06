module Day03 where

import Control.Applicative
import Data.Char
import Data.Maybe
import Text.ParserCombinators.ReadP hiding (many)
import Utils

type MulInstruction = (Int, Int)
type Input = [MulInstruction]
type Output = Int

upTo :: Int -> ReadP a -> ReadP [a]
upTo n p
  | n > 0 = (:) <$> p <*> upTo (n-1) p <|> pure []
  | otherwise = pure []

parser :: ReadP Input
parser = catMaybes <$> instructions
  where
    instructions = manyTill maybeInstruction eof
    maybeInstruction = justInstruction <++ nonInstruction
    justInstruction = pure <$> mulInstruction
    nonInstruction = Nothing <$ get
    mulInstruction = string "mul(" *> mulArgs <* char ')'
    mulArgs = liftA2 (,) mulArg $ char ',' *> mulArg
    mulArg = fmap read $ (:) <$> mulArgDigit <*> upTo 2 mulArgDigit
    mulArgDigit = satisfy isDigit

solution1 :: Solution Input Output
solution1 = sum . fmap (uncurry (*))

day03 :: IO ()
day03 = do
  input <- getInput "day03-input.txt"
  let solve = createSolver parser input
  solve solution1
