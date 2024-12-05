module Main where

import Day01 (day01)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    "1" : _ -> day01
    _ -> error "None or invalid day number provided."
