import Control.Monad
import Data.Bool
import Data.List

t2 :: ([a] -> [b]) -> [[a]] -> [[b]]
t2 f = transpose . map f . transpose

left,right :: [Bool] -> [Int]
left = tail . scanl (\p n -> bool 0 (p+1) n) 0
right = init . scanr (\n p-> bool 0 (p+1) n) 0

count :: Int -> Int -> Int -> Int -> Int
count l r u d = ells l u + ells u r + ells r d + ells d l

ells :: Int -> Int -> Int
ells x y = ells' x y + ells' y x
  where
    ells' short long = max 0 $ highest - smallest + 1 where
      smallest = 2
      highest = min short (long `div` 2)

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \tn -> do
    [r,_c] <- map read . words <$> getLine
    g <- replicateM r (map (toEnum . read) . words <$> getLine)
    let lss = map left g
        rss = map right g
        uss = t2 left g
        dss = t2 right g
    let answer = sum $ concat $ (zipWith4.zipWith4) count lss rss uss dss
    putStrLn $ "Case #" ++ show tn ++ ": " ++ show answer
