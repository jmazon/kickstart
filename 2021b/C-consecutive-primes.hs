import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    z <- readLn
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve z)

high :: Int
high = 1000000007

primes :: UArray Int Bool
primes = runSTUArray $ do
  ps <- newArray (2,high) True
  forM_ [2..31622] $ \p -> do
    is <- readArray ps p
    when is $ forM_ [2*p,3*p..high] $ \c -> writeArray ps c False
  pure ps

solve :: Int -> Int
solve z = bsearch 2 high where
  bsearch lo hi | hi <= lo+1 = lowP lo * lowP hi
                | lowP m * hiP m <= z = bsearch m hi
                | otherwise = bsearch lo m
    where m = (lo + hi) `div` 2
  lowP x = head [ p | p <- [x,x-1..], primes!p ]
  hiP x = head [ p | p <- [x+1,x+2..], primes!p ]
