import Control.Monad

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [_n,k] <- map read . words <$> getLine
    s <- getLine
    let g = sum (map fromEnum $ zipWith (/=) s (reverse s)) `div` 2
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (abs (k-g))
