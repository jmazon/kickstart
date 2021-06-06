import Control.Monad

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    void getLine -- n
    vs <- map read . words <$> getLine
    let answer = solve (-1) vs
    putStrLn $ "Case #" ++ show i ++ ": " ++ show answer

solve :: Int -> [Int] -> Int
solve m (p:vs @ ~(s:_))
  | p > m && (null vs || p > s) = 1 + solve p vs
  | otherwise = solve (max m p) vs
solve _ [] = 0
