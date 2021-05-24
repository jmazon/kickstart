import Data.Array
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \tn -> do
    [r,c] <- map read . words <$> getLine
    g <- listArray ((1,1),(r,c)) . concat <$> replicateM r (map read . words <$> getLine)
    let neighbors (i,j) = filter (inRange (bounds g)) [(i-1,j),(i+1,j),(i,j-1),(i,j+1)]
    let answer = runST $ do
          a <- thaw g :: ST s (STUArray s (Int,Int) Int)
          cost <- newSTRef 0
          let dfs [] = pure ()
              dfs (n:q) = do
                  h <- readArray a n
                  let ns = neighbors n
                  m <- maximum . (0 :) <$> mapM (readArray a) ns
                  if h < m-1
                    then do
                      writeArray a n (m-1)
                      modifySTRef' cost (+ (m-1-h))
                      dfs (ns ++ q)
                    else do
                      dfs q
          dfs (indices g)
          readSTRef cost
    putStrLn $ "Case #" ++ show tn ++ ": " ++ show answer
