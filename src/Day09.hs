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

mutateDisk :: MDisk s -> StateT (Int, Int) (ST s) Disk
mutateDisk disk = do
  (p, q) <- get
  if p >= q then V.freeze disk else
    let handleItems (Just (Just _)) _ = do
          put (p + 1, q)
          mutateDisk disk
        handleItems _ (Just Nothing) = do
          put (p, q - 1)
          mutateDisk disk
        handleItems (Just Nothing) (Just (Just _)) = do
          MV.swap disk p q
          put (p + 1, q - 1)
          mutateDisk disk
        handleItems _ _ = V.freeze disk
     in do
       p' <- MV.readMaybe disk p
       q' <- MV.readMaybe disk q
       handleItems p' q'

defrag :: Disk -> Disk
defrag disk = runST $ evalStateT mutation initialState
  where
    mutation = V.thaw disk >>= mutateDisk
    initialState = (0, length disk - 1)

checksum :: Disk -> Checksum
checksum = sum . zipWith (*) [0..] . V.toList . V.catMaybes

solution1 :: Solution Input Output
solution1 = checksum . defrag . readDiskMap

day09 :: IO ()
day09 = do
  input <- getInput "day09-input-example.txt"
  let solve = createSolver parser input
  solve solution1
