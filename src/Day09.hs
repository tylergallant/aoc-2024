{-# LANGUAGE LambdaCase #-}

module Day09 where

import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Char
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV
import Text.ParserCombinators.ReadP hiding (get)
import Utils

type FileId = Int
type FileLength = Int
type SpaceLength = Int
type DiskMap = [(FileId, FileLength, SpaceLength)]
type Block = Maybe FileId
type Disk = Vector Block
type MDisk s = MVector s Block
type Checksum = Int
type Input = DiskMap
type Output = Checksum

parser :: ReadP Input
parser = pDiskMap <* char '\n' <* eof
  where
    pDiskMap = zipWith addId [0..] . pairs <$> many readDigit
    addId fileId (fileLength, spaceLength) = (fileId, fileLength, spaceLength)
    readDigit = read . pure <$> satisfy isDigit
    pairs [] = []
    pairs [x] = [(x, 0)]
    pairs (x:y:xs) = (x, y) : pairs xs

readDiskMap :: DiskMap -> Disk
readDiskMap diskMap = V.fromList $ do
  (fid, fl, sl) <- diskMap
  replicate fl (Just fid) ++ replicate sl Nothing

mutate :: MDisk s -> StateT (Int, Int) (ST s) Disk
mutate disk = do
  (p, q) <- get
  if p >= q then V.freeze disk else do
    (,) <$> MV.readMaybe disk p <*> MV.readMaybe disk q >>= \case
      -- Outside bounds of vector, halt
      (Nothing, _) -> V.freeze disk
      (_, Nothing) -> V.freeze disk
      -- p is a file, continue
      (Just (Just _), _) -> do
        put (p + 1, q)
        mutate disk
      -- q is free space, continue
      (_, Just Nothing) -> do
        put (p, q - 1)
        mutate disk
      -- p is free space, q is a file, swap
      (Just Nothing, Just (Just _)) -> do
        MV.swap disk p q
        put (p + 1, q - 1)
        mutate disk

defrag :: Disk -> Disk
defrag disk = runST $ evalStateT mutatedDisk initialState
  where
    mutatedDisk = V.thaw disk >>= mutate
    initialState = (0, length disk - 1)

checksum :: Disk -> Checksum
checksum = sum . zipWith (*) [0..] . V.toList . V.catMaybes

solution1 :: Solution Input Output
solution1 = checksum . defrag . readDiskMap

day09 :: IO ()
day09 = do
  input <- getInput "day09-input.txt"
  let solve = createSolver parser input
  solve solution1
