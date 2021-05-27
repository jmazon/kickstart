import Control.Monad (forM_,replicateM)
import Data.List (sort,transpose)

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    n <- readLn
    [xs,ys] <- map sort . transpose <$> replicateM n (map read . words <$> getLine)
    let xs' = zipWith (-) (sort xs) [0..]
    let x = sort xs' !! (n `div` 2)
        y = sort ys !! (n `div` 2)
    let xCost = sum $ abs . (x -) <$> xs'
        yCost = sum $ abs . (y -) <$> ys
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (xCost + yCost :: Int)
