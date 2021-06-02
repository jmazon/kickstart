import Control.Monad hiding (ap)

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [n,a,b,c] <- map read . words <$> getLine
    let answer = solve n a b c
    putStrLn $ "Case #" ++ show i ++ ": " ++
      maybe "IMPOSSIBLE" (unwords . map show) answer

solve :: Int -> Int -> Int -> Int -> Maybe [Int]
solve n a b c =
    guard (ap + c + bp <= n) *>
    guard (mh >= 0) *>
    pure (incr ap ++ ah×1 ++ [n] ++ mh×1 ++ (c-1)×n ++ bh×1 ++ decr bp)
  where
    (×) = replicate
    ap = a - c
    bp = b - c
    i = n - c - ap - bp
    (ah,mh,bh) | i == 0 = (0,0,0)
               | c > 1 = (0,i,0)
               | ap > 0 && n > 2 = (i,0,0)
               | bp > 0 && n > 2 = (0,0,i)
               | otherwise = (-1,-1,-1)
    incr k = [n-k..n-1]
    decr = reverse . incr
