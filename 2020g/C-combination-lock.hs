{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Data.List as List
import Data.Vector (Vector,(!))
import qualified Data.Vector as V

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [w,n] <- map read . words <$> getLine
    xs <- map read . words <$> getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve w n xs)

solve :: Int -> Int -> [Int] -> Int
solve w n xs = minimum $
               map cost $
               List.take w $
               iterate rotate $
               flip make n xs

cost :: Data -> Int
cost Data{..} = 
    dPartials!i - dPartials!0
    - i * m
    + (dW - i) * (m + dN)
    - dPartials!dW + dPartials!i
  where
    m = V.head dSeq
    i = bsearch (\x -> 2 * (dSeq!x - m) > dN) 0 dW

-- “lower_bound”
bsearch :: (Int -> Bool) -> Int -> Int -> Int
bsearch p = go where
  go lo hi | hi <= lo + 1 = hi
           | p m = go lo m
           | otherwise = go m hi
    where m = (lo + hi) `div` 2

rotate :: Data -> Data
rotate Data{..} = Data
  { dSeq = V.tail dSeq
  , dW
  , dN
  , dPartials = V.tail dPartials
  }

data Data = Data
  { dSeq :: !(Vector Int)
  , dW :: !Int
  , dN :: !Int
  , dPartials :: !(Vector Int)
  }
  deriving Show

make :: [Int] -> Int -> Data
make xs dN = Data{..}
  where
    xs' = sort xs
    dSeq = V.fromList (xs' ++ map (+dN) xs')
    dW = length xs
    dPartials = V.scanl (+) 0 dSeq
