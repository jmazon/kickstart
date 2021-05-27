{-# LANGUAGE TupleSections #-}
import Control.Monad
import Data.Array
import Data.Array.ST
import Data.Char (ord)

toNum :: String -> [Int]
toNum = map (\c -> ord c - ord 'A')

toAdj :: [Int] -> [(Int,Int)]
toAdj [] = []
toAdj (c:cs) = map (c,) cs ++ map (,c) cs ++ toAdj cs

fw :: Array (Int,Int) Bool -> Array (Int,Int) Int
fw a = runSTArray $ do
  r <- newArray ((0,0),(25,25)) (maxBound `div` 2)
  forM_ (assocs a) $ \((i,j),conn) -> do
    when conn $ writeArray r (i,j) 1
  forM_ [0..25] $ \i -> writeArray r (i,i) 0
  forM_ [0..25] $ \k -> do
    forM_ [0..25] $ \i -> do
      forM_ [0..25] $ \j -> do
        direct <- readArray r (i,j)
        indirect <- (+) <$> readArray r (i,k) <*> readArray r (k,j)
        when (indirect < direct) $
          writeArray r (i,j) indirect
  pure r

solve :: Array (Int,Int) Int -> [[Int]] -> Int
solve adj [xs,ys]
  | m < 26 = m + 2
  | otherwise = -1
  where m = minimum [ adj ! (i,j) | i <- xs, j <- ys ]

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [n,q] <- map read . words <$> getLine
    ss <- listArray (1,n) . map toNum . words <$> getLine
    let adj = accumArray (||) False ((0,0),(25,25)) . map (,True) $
              concatMap toAdj $ elems ss
        adj' = fw adj
    answer <- replicateM q (solve adj' . map ((ss !) . read) . words <$> getLine)
    putStrLn $ "Case #" ++ show i ++ ": " ++ unwords (map show answer)
