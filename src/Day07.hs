module Day07 where

import Control.Applicative
import Text.ParserCombinators.ReadP
import Utils

type TestValue = Int
type Operand = Int
type Equation = (TestValue, [Operand])
type Operator = Int -> Int -> Int
type Input = [Equation]
type Output = Int

pInt :: ReadP Int
pInt = readS_to_P reads

pEquation :: ReadP Equation
pEquation = (,) <$> pTestValue <*> pOperands
  where
    pTestValue = pInt <* string ": "
    pOperands = sepBy pInt $ char ' '

parser :: ReadP Input
parser = manyTill lineSeparatedEquation eof
  where lineSeparatedEquation = pEquation <* char '\n'

candidates :: [Operand] -> [[Operator]]
candidates = go . subtract 1 . length
  where
    go n
      | n <= 0 = []
      | n == 1 = fmap (:[]) operators
      | otherwise = liftA2 (:) operators . go $ n - 1
    operators = [(+), (*)]

possibleResults :: [Operand] -> [TestValue]
possibleResults operands = go operands <$> candidates operands
  where
    go [] _ = 0
    go [x] _ = x
    go (x:_:_) [] = x
    go (x:y:xs) (op:ops) = go (op x y : xs) ops

couldBeTrue :: Equation -> Bool
couldBeTrue (testValue, operands) = elem testValue $ possibleResults operands

solution1 :: Solution Input Output
solution1 = sum . fmap fst . filter couldBeTrue

day07 :: IO ()
day07 = do
  input <- getInput "day07-input.txt"
  let solve = createSolver parser input
  solve solution1
