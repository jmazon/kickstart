{-# LANGUAGE MultiWayIf #-}

import           Control.Monad
import           Control.Monad.ST
import           Data.Function
import           Data.Map.Strict (Map)
import           Data.Set        (Set)
import           Data.Vector     (Vector)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [n,m,k] <- map read . words <$> getLine
    as <- V.fromListN k <$> replicateM k readLn
    let answer = iterate (unroll m as) (Map.singleton as 0) !! n
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (answer Map.! V.empty)

type N = Vector Int

unroll :: Int -> Vector Int -> Map N Double -> Map N Double
unroll m as mp = Map.fromSet compute $ foldMap id $ Set.map unplay $ Map.keysSet mp
  where
    compute :: N -> Double
    compute v = (fromIntegral m + V.sum (V.map (mp Map.!) v's) + fromIntegral news * Map.findWithDefault 0 new mp) / fromIntegral (length v's + news)
      where v's = V.filter (valid as) (play v)
            news = fromEnum (valid as new) * (m - V.length v)
            new = V.cons 1 v

play :: N -> V.Vector N
play v = flip V.imap v $ \i0 c -> runST $ do
  v' <- V.thaw v
  let c' = c+1
      n = V.length v
  MV.write v' i0 c'
  flip fix i0 $ \loop i -> when (i < n-1) $ do
    cmp <- MV.read v' (i+1)
    when (cmp < c') $ do
      MV.swap v' i (i+1)
      loop (i+1)
  V.unsafeFreeze v'

unplay :: N -> Set N
unplay v = Set.fromList $ flip map (V.toList (V.indexed v)) $ \(i0,c) -> runST $ do
  v' <- V.thaw v
  let c' = c - 1
  MV.write v' i0 c'
  v'' <- flip fix i0 $ \loop i ->
          if | i > 0 -> do
                 cmp <- MV.read v' (i-1)
                 if cmp > c' then MV.swap v' i (i-1) *> loop (i-1) else pure v'
             | c' == 0 -> pure (MV.tail v')
             | otherwise -> pure v'
  V.unsafeFreeze v''

valid :: N -> N -> Bool
valid = go where
  go as ns
    | V.null ns = True
    | V.null as = False
    | V.head ns <= V.head as = go (V.tail as) (V.tail ns)
    | otherwise = go (V.tail as) ns
