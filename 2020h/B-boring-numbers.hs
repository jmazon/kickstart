import Control.Monad
import Data.Char

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [l,r] <- map read . words <$> getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve r - solve (l-1))

solve :: Int -> Int
solve n = go (digitToInt <$> show n) where
  go ds = onOdd ds + dropping (tail ds)
  onOdd (d:ds) = (d `div` 2) * complete ds      -- lower odds × all smaller
                 + fromEnum (odd d) * onEven ds -- equal
  onOdd [] = 1
  onEven (d:ds) = ((d+1) `div` 2) * complete ds -- lower evens (>0) × all smaller
               + fromEnum (even d) * onOdd ds   -- equal
  onEven [] = 1
  complete = product . map (const 5)
  dropping [] = 0
  dropping ds = complete ds + dropping (tail ds)
