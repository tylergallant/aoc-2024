module Day03 where

import Control.Applicative
import Data.Char
import Data.Maybe
import Text.ParserCombinators.ReadP hiding (many)
import Utils

type MulInstruction = (Int, Int)
data Instruction = Mul MulInstruction | Do | Dont
type Input = [Instruction]
type Output = Int

pUpTo :: Int -> ReadP a -> ReadP [a]
pUpTo n p
  | n > 0 = (:) <$> p <*> pUpTo (n-1) p <|> pure []
  | otherwise = pure []

pMulInstruction :: ReadP Instruction
pMulInstruction = fmap Mul $ string "mul(" *> mulArgs <* char ')'
  where
    mulArgs = liftA2 (,) mulArg $ char ',' *> mulArg
    mulArg = fmap read $ (:) <$> mulArgDigit <*> pUpTo 2 mulArgDigit
    mulArgDigit = satisfy isDigit

pDoInstruction :: ReadP Instruction
pDoInstruction = Do <$ string "do()"

pDontInstruction :: ReadP Instruction
pDontInstruction = Dont <$ string "don't()"

pInstruction :: ReadP Instruction
pInstruction = pMulInstruction <|> pDoInstruction <|> pDontInstruction

parser :: ReadP Input
parser = catMaybes <$> instructions
  where
    instructions = manyTill maybeInstruction eof
    maybeInstruction = justInstruction <++ nonInstruction
    justInstruction = pure <$> pInstruction
    nonInstruction = Nothing <$ get

isMul :: Instruction -> Bool
isMul (Mul _) = True
isMul _ = False

onlyMuls :: [Instruction] -> [MulInstruction]
onlyMuls = mapMaybe maybeMul
  where
    maybeMul Do = Nothing
    maybeMul Dont = Nothing
    maybeMul (Mul m) = Just m

filterDonts :: [Instruction] -> [Instruction]
filterDonts [] = []
filterDonts (Do:xs) = let (ms, rest) = span isMul xs in ms ++ filterDonts rest
filterDonts (Dont:xs) = filterDonts $ dropWhile isMul xs
filterDonts xs = filterDonts $ Do : xs

solution1 :: Solution Input Output
solution1 = sum . fmap (uncurry (*)) . onlyMuls

solution2 :: Solution Input Output
solution2 = solution1 . filterDonts

day03 :: IO ()
day03 = do
  input <- getInput "day03-input.txt"
  let solve = createSolver parser input
  solve solution1
  solve solution2
