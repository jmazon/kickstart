import Control.Monad.ST
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M

readInt :: B.ByteString -> Int
readInt s = i where Just (i,_) = B.readInt s

main :: IO ()
main = do
  t <- readInt <$> B.getLine
  V.forM_ (V.enumFromTo 1 t) $ \tn -> do
    n <- readInt <$> B.getLine
    as <- V.fromListN n . map readInt . B.words <$> B.getLine
    let ps = V.scanl (+) 0 as
        offset = V.minimum ps
        sz = V.maximum ps - offset + 1
        pfxs0 = V.accum (+) (V.replicate sz 0) [(negate offset,1)]
    let (_,answer) = runST $ do
          pfxs <- V.unsafeThaw pfxs0
          V.foldM' (step offset pfxs) (0,0) as
    putStrLn $ "Case #" ++ show tn ++ ": " ++ show answer

squares :: V.Vector Int
squares = V.fromList $ takeWhile (<= 10000000) [ x*x | x <- [0..] ]

step :: Int -> V.MVector s Int -> (Int,Int) -> Int -> ST s (Int,Int)
step offset prefixes (prefixSum,count) a = do
  let prefixSum' = prefixSum + a
  count' <- (count +) . V.sum <$>
            V.mapM (\cp -> M.read prefixes (cp - offset))
            (V.takeWhile (>= offset) $ V.map (prefixSum' -) squares)
  M.modify prefixes (+ 1) (prefixSum' - offset)
  count' `seq` pure (prefixSum',count')
