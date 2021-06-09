import Control.Monad (forM_)

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [_n,k] <- map read . words <$> getLine
    ds <- (zipWith (-) =<< tail) . map read . words <$> getLine
    let answer = bsearch (not . feasible k ds) 0 (maximum ds)
    putStrLn $ "Case #" ++ show i ++ ": " ++ show answer

bsearch :: (Int -> Bool) -> Int -> Int -> Int
bsearch p = go where
  go l h | h <= l+1 = h
         | p m = go m h
         | otherwise = go l m
    where m = (l + h + 1) `div` 2

feasible :: Int -> [Int] -> Int -> Bool
feasible k ds target = sum (map numInserts ds) <= k
  where numInserts d = (d + target - 1) `div` target - 1
