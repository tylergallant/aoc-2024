module Day07 where

import Data.Foldable
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

concatDigits :: Int -> Int -> Int
concatDigits x y = read $ show x ++ show y

couldBeTrue :: [Operator] -> Equation -> Bool
couldBeTrue _ (_, []) = False
couldBeTrue _ (target, [n]) = target == n
couldBeTrue ops (target, n:ns) = elem target $ foldl' f [n] ns
  where f rs o = [op r o | r <- rs, op <- ops, op r o <= target]

solution1 :: Solution Input Output
solution1 = sum . fmap fst . filter criteria
  where criteria = couldBeTrue [(+), (*)]

solution2 :: Solution Input Output
solution2 = sum . fmap fst . filter criteria
  where criteria = couldBeTrue [(+), (*), concatDigits]

day07 :: IO ()
day07 = do
  input <- getInput "day07-input.txt"
  let solve = createSolver parser input
  solve solution1
  solve solution2
