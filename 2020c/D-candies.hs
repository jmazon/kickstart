{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
import Control.Monad
import Data.Bits
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [n,q] <- map read . words <$> getLine
    as <- V.fromListN n . map read . words <$> getLine
    candies <- newCandies as
    answer <- fmap sum $ replicateM q $ do
      (op:ops) <- words <$> getLine
      case op of
        "U" -> do
          let [x,v] = map read ops
          update candies (fromIntegral (x-1)) v
          pure 0
        "Q" -> do
          let [l,r] = map read ops
          query candies (l-1) (r-1)
    putStrLn $ "Case #" ++ show i ++ ": " ++ show answer

data Candies = Candies {
    cScaled :: FenwickTree Int
  , cFlat :: FenwickTree Int
  }

newCandies :: V.Vector Int -> IO Candies
newCandies v = do
  cScaled <- ftNew (V.imap (\i a -> (-1)^fromIntegral i * fromIntegral (i+1) * a) v)
  cFlat <- ftNew (V.imap (\i a -> (-1)^i * a) v)
  pure Candies{..}

update :: Candies -> Int -> Int -> IO ()
update Candies{..} i a = do
  ftSet cScaled i $ (-1)^fromIntegral i * fromIntegral (i+1) * a
  ftSet cFlat i $ (-1)^i * a

query :: Candies -> Int -> Int -> IO Int
query Candies{..} l r = do
  scaled <- ftRangeSum cScaled l (r+1)
  flat <- ftRangeSum cFlat l (r+1)
  let sign = (-1)^l
  pure $ sign * (scaled - fromIntegral l * flat)

newtype FenwickTree a = FT (M.IOVector a)

lsBit :: Int -> Int
lsBit i = i .&. negate i

ftNew :: Num a => V.Vector a -> IO (FenwickTree a)
ftNew v = do
  ft <- V.thaw v
  let n = V.length v
  V.forM_ (V.enumFromTo 0 (n - 1)) $ \i -> do
    let j = i + lsBit (i+1)
    when (j < n) $ flip (M.modify ft) j . (+) =<< M.read ft i
  pure (FT ft)

ftAdd :: Num a => FenwickTree a -> Int -> a -> IO ()
ftAdd (FT ft) i delta = go i where
  go i | i >= M.length ft = pure ()
       | otherwise = M.modify ft (+ delta) i *> go (i + lsBit (i+1))

ftRangeSum :: Num a => FenwickTree a -> Int -> Int -> IO a
ftRangeSum (FT ft) i0 j0 = go j0 0 where
  go j s | j <= i0 = go' j i0 s
         | otherwise = do
             s' <- (s +) <$> M.read ft (j-1)
             go (j - lsBit j) $! s'
  go' j i s | i <= j = pure s
            | otherwise = do
                s' <- (s -) <$> M.read ft (i-1)
                go' j (i - lsBit i) $! s'

ftGet :: Num a => FenwickTree a -> Int -> IO a
ftGet ft i = ftRangeSum ft i (i+1)

ftSet :: Num a => FenwickTree a -> Int -> a -> IO ()
ftSet ft i v = ftAdd ft i . (v -) =<< ftGet ft i
