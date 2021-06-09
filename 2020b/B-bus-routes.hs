import Control.Monad (forM_)

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [_n,d] <- map read . words <$> getLine
    xs <- map read . words <$> getLine
    let answer = foldr (\x e -> e - e `mod` x) d xs :: Int
    putStrLn $ "Case #" ++ show i ++ ": " ++ show answer
