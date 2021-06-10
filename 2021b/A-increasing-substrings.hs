import Control.Monad

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    void getLine
    s <- getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ unwords (map show (solve s))

solve :: String -> [Int]
solve = go ' ' 0 where
  go p l (c:cs) | c <= p = 1 : go c 1 cs
                | otherwise = let l' = l+1 in l' : go c l' cs
  go _ _ [] = []
