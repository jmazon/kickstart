import Control.Monad

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [_n,k] <- map read . words <$> getLine
    as <- map read . words <$> getLine
    let answer = solve k as
    putStrLn $ "Case #" ++ show i ++ ": " ++ show answer

solve :: Int -> [Int] -> Int
solve k = go where
  go (x:xs) | x == k = count (k-1) xs
            | otherwise = go xs
  go [] = 0
  count 0 xs = 1 + go xs
  count i (x:xs) | x == i = count (i-1) xs
                 | otherwise = go (x:xs)
  count _ [] = 0
