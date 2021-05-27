import Control.Monad

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [n,k,s] <- map read . words <$> getLine :: IO [Int]
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (min (n+k) (n+2*(k-s)))
