import Control.Monad (forM_)
import Data.Bifunctor (first)
import Data.List (sort)

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [_n,x] <- map read . words <$> getLine :: IO [Int]
    let turns a = (a + x - 1) `div` x
    as <- map read . words <$> getLine
    let answer = map snd $ sort $ map (first turns) $ zip as [1 :: Int ..]
    putStrLn $ "Case #" ++ show i ++ ": " ++ unwords (map show answer)
