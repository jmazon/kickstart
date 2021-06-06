import Control.Monad

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    void getLine -- k
    as <- map read . words <$> getLine
    let answer = solve as
    putStrLn $ "Case #" ++ show i ++ ": " ++ show answer

solve :: [Int] -> Int
solve = fst . foldl go (0,(0 :: Int,0 :: Int)) . (zipWith compare =<< tail)
  where
    go x EQ = x
    go (n,(lo,_)) GT = check (n,(lo+1,0))
    go (n,(_,hi)) LT = check (n,(0,hi+1))
    check (n,(lo,hi)) | lo + hi > 3 = (n+1,(0,0))
    check x = x
