import Control.Monad (forM_,void)
import Data.List (group,tails)

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    void getLine
    as <- map read . words <$> getLine
    let pgs = progressions as
        candidates = concatMap merges (tails pgs)
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (maximum candidates)

progressions :: [Int] -> [[Int]]
progressions xs = group $ zipWith (-) (tail xs) xs

merges :: [[Int]] -> [Int]
merges (g1:[d2]:[d3]:g4:_)
  | head g4 == head g1, d2 + d3 == 2 * head g1 = [length g1 + 3 + length g4]
merges (g1:[d2]:g3:_)
  | d2 + head g3 == 2 * head g1 = [length g1 + 3]
  | d2 + head g1 == 2 * head g3 = [length g3 + 3]
merges (g1:g2:_) = [max (length g1) (length g2) + 2]
merges [g] = [length g + 1]
merges [] = []
