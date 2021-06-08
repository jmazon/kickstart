import Control.Monad (forM_,replicateM)
import Data.List (sort)

readPair :: String -> (Int,Int)
readPair s = (x,y) where [x,y] = map read (words s)

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [n,k] <- map read . words <$> getLine
    is <- replicateM n (readPair <$> getLine)
    let answer = fst $ (\f -> foldl f (0,0) (sort is)) $
          \(c,f) (s,e) ->
            let vs = max s f
                q = (e-vs+k-1) `div` k
                c' = max 0 q
            in (c+c',vs+c'*k)
    putStrLn $ "Case #" ++ show i ++ ": " ++ show answer
