import Control.Monad (forM_,void)
import Data.List (tails)

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    void getLine -- n
    hs <- map read . words <$> getLine
    let answer = length (filter peak (tails hs))
    putStrLn $ "Case #" ++ show i ++ ": " ++ show answer

peak :: [Int] -> Bool
peak (a:b:c:_) = b > max a c
peak _ = False
