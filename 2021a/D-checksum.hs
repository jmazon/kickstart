{-# LANGUAGE TupleSections #-}

import Control.Monad (forM_,replicateM,guard,void)
import Data.Array (Array,listArray,(!),elems,assocs)
import Data.Coerce (coerce)
import Data.Function (fix)
import Data.List (unfoldr)
import Data.Maybe (catMaybes)
import Data.Monoid (Sum(Sum))
import Data.Ord (Down(Down),comparing)
import Data.Semigroup (Min(Min))
import qualified Data.Set as S

import Control.Monad.ST
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \tn -> do
    n <- readLn
    a <- replicateM n (map (== "-1") . words <$> getLine)
    b <- listArray ((1,1),(n,n)) . concat .
        (zipWith.zipWith) (\x y -> guard x *> Just y) a <$>
        replicateM n (map read . words <$> getLine)
    void getLine -- r
    void getLine -- c
    let answer = solve b
    putStrLn $ "Case #" ++ show tn ++ ": " ++ show answer

solve :: Array (Int,Int) (Maybe Int) -> Int
solve c = sum (catMaybes (elems c)) - coerce (mconcat (unfoldr go es0)) where
  cost (Left a) (Right b) = Down . Sum <$> c!(a,b)
  cost (Right b) (Left a) = Down . Sum <$> c!(a,b)
  cost _ _ = Nothing
  es0 = nubOrd $ concat [ [Left i,Right j] | ((i,j),Just _) <- assocs c ] :: [Either Int Int]
  go [] = Nothing
  go vs = Just (prim cost vs)

prim :: (Show weight,Ord weight,Num weight) => (v -> v -> Maybe weight) -> [v] -> (weight,[v])
prim cost vs0 = runST $ do
  totalWeight <- newSTRef 0
  let vs = V.fromList vs0
  let n = V.length vs
  selected <- V.unsafeThaw (V.replicate n False)
  let minE0 = V.cons (Just (Min 0)) (V.replicate (n-1) Nothing)
  flip fix minE0 $ \loop minE -> do
    let ime = V.indexed minE
        ime' = V.mapMaybe (\(i,mbMW) -> (i,) <$> mbMW) ime
    ime'' <- V.filterM (fmap not . MV.read selected . fst) ime'
    if V.null ime''
      then do
        tw <- readSTRef totalWeight
        r <- V.toList . V.map fst . V.filter (not . snd) . V.zip vs <$> V.unsafeFreeze selected
        pure (tw,r)
      else do
        let (i,Min w) = V.minimumBy (comparing snd) ime''
        MV.write selected i True
        modifySTRef' totalWeight (+ w)
        let v = vs V.! i
        loop $ V.imap (\to mMbW -> (mMbW <>) $ fmap Min $ (cost v (vs V.! to))) minE

nubOrd :: Ord a => [a] -> [a]
nubOrd = S.toList . S.fromList
