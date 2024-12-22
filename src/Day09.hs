module Day09 where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Char (isDigit)
import Data.Heap (Heap, empty, insert, viewMin, deleteMin)
import Data.List (mapAccumL, sortOn)
import Data.Maybe (fromMaybe, isNothing, catMaybes, listToMaybe)
import Data.Vector (Vector, fromList, toList, freeze, thaw, accum)
import Data.Vector.Mutable (MVector, readMaybe, swap, modify)
import qualified Data.Vector.Mutable as MV (read, length)
import Text.ParserCombinators.ReadP (ReadP, eof, char, satisfy, many)
import Utils (Solution, getInput, createSolver)

type FileId = Int
type FileLength = Int
type SpaceLength = Int
type FileLocation = Int
type Checksum = Int
type DiskMap = [(FileId, FileLength, SpaceLength)]
type Block = Maybe FileId
type Chunk = (FileLength, Block)
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

readBlocks :: DiskMap -> Vector Block
readBlocks diskMap = fromList $ do
  (fid, fl, sl) <- diskMap
  replicate fl (Just fid) ++ replicate sl Nothing

mutateBlocks :: Int -> Int -> MVector s Block -> ST s (Vector Block)
mutateBlocks p q disk
  | p >= q = freeze disk
  | otherwise = (,) <$> readDisk p <*> readDisk q >>= uncurry handleBlocks
  where
    readDisk = readMaybe disk
    handleBlocks (Just (Just _)) _ = mutateBlocks (p + 1) q disk
    handleBlocks _ (Just Nothing) = mutateBlocks p (q - 1) disk
    handleBlocks (Just Nothing) (Just (Just _)) = do
      swap disk p q
      mutateBlocks (p + 1) (q - 1) disk
    handleBlocks _ _ = freeze disk

moveBlocks :: Vector Block -> Vector Block
moveBlocks disk = runST $ thaw disk >>= mutateBlocks p q
  where
    p = 0
    q = length disk - 1

checksum :: Vector Block -> Checksum
checksum = sum . zipWith (*) [0..] . toList . fmap (fromMaybe 0)

solution1 :: Solution Input Output
solution1 = checksum . moveBlocks . readBlocks




readChunks :: DiskMap -> [(FileLocation, Chunk)]
readChunks = concat . snd . mapAccumL f 0
  where f i (x, y, z) = (i + y + z, filter (\(_, (l, _)) -> l > 0) [(i, (y, Just x)), (i + y, (z, Nothing))])

freeChunksBySize :: [(FileLocation, Chunk)] -> Vector (Heap Int)
freeChunksBySize disk = accum addLocation heaps $ lengthAndLocation <$> spaces
  where
    isSpace = isNothing . snd . snd
    spaces = filter isSpace disk
    maxSize = maximum $ fst . snd <$> spaces
    heaps = fromList $ replicate (maxSize + 1) empty
    lengthAndLocation (loc, (len, _)) = (len, loc)
    addLocation = flip insert

spaceCandidate
  :: MVector s (Heap Int)
  -> FileLength
  -> FileLocation
  -> ST s (Maybe (FileLength, FileLocation))
spaceCandidate heaps minLen loc = getBest . filterCandidates <$> candidates
  where
    getBest = listToMaybe . sortOn snd
    lengths = [minLen .. MV.length heaps - 1]
    getCandidate len = fmap ((,) len . fst) . viewMin <$> MV.read heaps len
    candidates = traverse getCandidate lengths
    filterCandidates = filter ((< loc) . snd) . catMaybes

mutateChunks :: MVector s (Heap Int) -- spaces
  -> (FileLocation, FileLength, FileId) -- files
  -> ST s (FileLocation, Chunk) -- result
mutateChunks spaces (loc, len, fid) = do
  candidate <- spaceCandidate spaces len loc
  case candidate of
    Nothing -> pure (loc, (len, Just fid))
    Just (cLen, cLoc) -> do
      modify spaces deleteMin cLen
      when (cLen > len) $ modify spaces (insert $ cLoc + len) $ cLen - len
      pure (cLoc, (len, Just fid))

files :: [(FileLocation, Chunk)] -> [(FileLocation, FileLength, FileId)]
files chunks = [(loc, len, fileId) | (loc, (len, Just fileId)) <- chunks]

moveChunks :: [(FileLocation, Chunk)] -> [(FileLocation, Chunk)]
moveChunks chunks = runST $ do
  spaces <- thaw $ freeChunksBySize chunks
  let fileQueue = reverse $ files chunks
  traverse (mutateChunks spaces) fileQueue

checksum' :: [(FileLocation, Chunk)] -> Int
checksum' = sum . map score
  where
    score (_, (_, Nothing)) = 0
    score (loc, (len, Just fid)) = fid * sum [loc..loc+len-1]

solution2 :: Solution Input Output
solution2 = checksum' . moveChunks . readChunks

day09 :: IO ()
day09 = do
  input <- getInput "day09-input.txt"
  let solve = createSolver parser input
  solve solution1
  solve solution2
