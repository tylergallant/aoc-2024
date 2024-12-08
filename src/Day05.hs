module Day05 where

import Data.Functor
import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP
import Utils

type Page = Int
newtype Rule = Rule { runRule :: Page -> Page -> Ordering }
type Update = [Page]
type Input = (Rule, [Update])
type Output = Int

instance Semigroup Rule where
  Rule r1 <> Rule r2 = Rule $ \x y -> r1 x y <> r2 x y

instance Monoid Rule where
  mempty = Rule $ \_ _ -> EQ

rule :: Page -> Page -> Rule
rule first second = Rule f
  where
    f x y
      | x == first && y == second = LT
      | x == second && y == first = GT
      | otherwise = EQ

pPage :: ReadP Page
pPage = readS_to_P reads

pRule :: ReadP Rule
pRule = rule <$> pPage <*> page2
  where
    sep = void $ char '|'
    page2 = sep *> pPage <* char '\n'

pUpdate :: ReadP Update
pUpdate = sepBy pPage sep <* char '\n'
  where sep = char ','

parser :: ReadP Input
parser = do
  rules <- manyTill pRule $ char '\n'
  updates <- manyTill pUpdate eof
  return (mconcat rules, updates)

isCorrectlyOrdered :: Rule -> Update -> Bool
isCorrectlyOrdered r update = update == sortBy sorting update
  where sorting = runRule r

middlePageNumber :: Update -> Maybe Int
middlePageNumber pages
  | length pages < 3 = listToMaybe pages
  | otherwise = middlePageNumber . tail $ init pages

solution1 :: Solution Input Output
solution1 (rules, updates) = sum middles
  where
    middles = mapMaybe middlePageNumber onlyCorrect
    onlyCorrect = filter correctByRules updates
    correctByRules = isCorrectlyOrdered rules

day05 :: IO ()
day05 = do
  input <- getInput "day05-input.txt"
  let solve = createSolver parser input
  solve solution1
