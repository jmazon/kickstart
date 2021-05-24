import Data.List
import Control.Monad

main :: IO ()
main = do
  t <- readLn:: IO Int
  forM_ [1..t] $ \i -> do
    [n,k] <- map read . words <$> getLine
    s <- map (subtract (fromEnum 'a') . fromEnum ) <$> getLine
    let pref = take ((n+1) `div` 2) s
        strict = foldl1' (\a b -> (k*a + b) `mod` m) pref
        s' = pref ++ reverse (take (n `div` 2) s)
        lax = fromEnum (s' < s)
    putStrLn $ "Case #" ++ show i ++ ": " ++ show ((strict + lax) `mod` m)

m :: Int
m = 1000000007
