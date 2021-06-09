import Control.Monad (forM_)
import GHC.Float (log1p)
import Data.Vector (Vector)
import qualified Data.Vector as V

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [w,h,l,u,r,d] <- map read . words <$> getLine
    let answer = solve w h l u r d
    putStrLn $ "Case #" ++ show i ++ ": " ++ show answer

solve :: Int -> Int -> Int -> Int -> Int -> Int -> Double
solve w h l u r d
  | min w h == 1 = 0
  | d == h = sum [ pReach w h (u-i) (r+i) | i <- [1.. (u-1) ] ]
  | r == w = sum [ pReach w h (d+j) (l-j) | j <- [1.. (l-1) ] ]
  | otherwise = 1 - fromAbove - fromLeft
  where fromAbove = sum [ 0.5 * pReach w h (u-1) j | j <- [l..r] ]
        fromLeft = sum [ 0.5 * pReach w h i (l-1) | i <- [u..d] ]

logFact,logPow2 :: Vector Double
logFact = V.scanl (+) 0 (V.generate 200000 (log1p . fromIntegral))
logPow2 = V.scanl (+) 0 (V.replicate 200000 (log 2))

nCr' :: Int -> Int -> Int -> Double
nCr' n k dv = exp (logFact V.! n - logFact V.! (n-k) - logFact V.! k - logPow2 V.! dv)

pReach :: Int -> Int -> Int -> Int -> Double
pReach w h i j
  | i < 1 || j < 1 = 0
  | otherwise = nCr' (i'-1+j'-1) (i'-1) (i-1+j-1)
  where i' = min i h
        j' = min j w
