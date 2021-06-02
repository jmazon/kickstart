import Control.Monad
import Data.List

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    void getLine
    as <- map read . words <$> getLine :: IO [Int]
    let answer = succ $ maximum $ map length $ group $ (zipWith (-) =<< tail) as
    putStrLn $ "Case #" ++ show i ++ ": " ++ show answer
