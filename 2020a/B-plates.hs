import           Control.Monad (forM_,replicateM)
import           Data.Vector   ((!?))
import qualified Data.Vector as V

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \tn -> do
    [n,k,p] <- map read . words <$> getLine
    pss <- replicateM n (V.fromListN k . map read . words <$> getLine)
    let dp = foldl f (V.singleton 0) pss
        f v ps = V.generate (min (p+1) (V.length v + k)) $
                 \i -> V.maximum $
                      V.imap (\a va -> maybe 0 (va +) (s !? (i-a))) $
                      V.take (i+1) v
          where s = V.scanl (+) 0 ps
    let answer = V.last dp :: Int
    putStrLn $ "Case #" ++ show tn ++ ": " ++ show answer
