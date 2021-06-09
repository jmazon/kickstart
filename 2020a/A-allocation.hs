import Control.Monad (forM_)
import Data.List     (sort)

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [_n,b] <- map read . words <$> getLine
    as <- map read . words <$> getLine :: IO [Int]
    putStrLn $ "Case #" ++ show i ++ ": " ++
      show (length $ takeWhile (<= b) $ scanl1 (+) $ sort as)
