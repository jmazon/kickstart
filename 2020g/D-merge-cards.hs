{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Vector.Unboxed (Vector,(!))
import qualified Data.Vector as BV
import qualified Data.Vector.Mutable as BMV
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    n <- readLn :: IO Int
    let solve | n <= 9 = const brute
              | n <= 100 = dp
              | otherwise = smart
    answer <- solve n . map read . words <$> getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show answer

brute :: [Double] -> Double
brute = avg . flip go 0 where
  go [_] s = pure s
  go as s = join [ go (h++[x+y]++t) $! s + x + y | (h,(x:y:t)) <- zip (inits as) (tails as) ]

avg :: [Double] -> Double
avg = uncurry (/) . foldl' go (0,0)
  where go (!s,!c) n = (s+n,c+1)
                          
dp :: Int -> [Double] -> Double
dp n as = runST $ do
  let a = V.fromListN n as
      p = V.scanl (+) 0 a
  v <- BMV.new (n+1) :: ST s (BMV.MVector s (Vector Double))
  BMV.write v 0 (error "No 0 in v")
  BMV.write v 1 (V.replicate n 0)
  forM_ [2..n] $ \l -> do
    v' <- MV.new (n-l+1)
    forM_ [0..n-l] $ \i -> do
      let t = p!(i+l) - p!i
      s <- forM [1..l-1] $ \w -> do
        x <- (! i) <$> BMV.read v w
        y <- (! (i+w)) <$> BMV.read v (l-w)
        pure (x+y)
      MV.write v' i (t + sum s / fromIntegral (l-1))
    BMV.write v l =<< V.unsafeFreeze v'
  (! 0) <$> BMV.read v n

smart :: Int -> [Double] -> Double
smart qn as = sum $ zipWith (*) as (V.toList (smartDp BV.! qn))

smartDp :: BV.Vector (Vector Double)
smartDp = BV.iterateN 5001 gen V.empty where
  gen prev
    | V.null prev = V.singleton 0
    | V.length prev == 1 = V.fromList [1,1]
    | otherwise =
      let n = V.length prev + 1 in
      V.map (/ (fromIntegral n-1)) $
      V.zipWith (+) (V.accum (-) (V.replicate n 2) [(0,1),(n-1,1)]) $
      V.zipWith (+)
        (V.imap (\i k -> (fromIntegral n-1-fromIntegral i) * k) (V.snoc prev 0))
        (V.imap (\i k -> fromIntegral i * k) (V.cons 0 prev))
