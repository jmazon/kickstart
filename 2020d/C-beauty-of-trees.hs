{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

import Control.Monad
import Data.Array
import Data.Ratio
import Data.Tree
import Data.Sequence ((<|),(|>),ViewL((:<)))
import qualified Data.Sequence as Q

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \tn -> do
    [n,a,b] <- map read . words <$> getLine
    parents <- map read . words <$> getLine
    let children = accumArray (flip (:)) [] (1,n) (zip parents [2..])
        nodes = array (1,n) [ (i,Node () (map (nodes!) (children!i))) | i <- [1..n] ]
        tree = nodes ! 1
    let answer = foldTree (solve (1 % fromIntegral n) a b) tree
    putStrLn $ "Case #" ++ show tn ++ ": "
      ++ show (fromRational (foldExp answer) :: Double)

type R = Rational

data Fold = Fold
  { foldPA :: !(Rep R)
  , foldPB :: !(Rep R)
  , foldExp :: !R
  }
  deriving Show

solve :: R -> Int -> Int -> a -> [Fold] -> Fold
solve unit a b _ [] = Fold
    { foldPA = Rep (unit <| Q.replicate (a-1) 0)
    , foldPB = Rep (unit <| Q.replicate (b-1) 0)
    , foldExp = 2*unit - unit*unit
    }
solve unit _ _ _ fs = Fold
    { foldPA = pA
    , foldPB = pB
    , foldExp = expected
    }
  where
    pA = pure unit + sum (eShift . foldPA <$> fs)
    pB = pure unit + sum (eShift . foldPB <$> fs)

    pAny = get pA + get pB - get pA * get pB

    expected = pAny + sum (foldExp <$> fs)

newtype Rep a = Rep (Q.Seq a) deriving (Show,Functor,Foldable)

instance Applicative Rep where
  pure x = Rep (Q.singleton x)
  (<*>) = error "(<*>)"

instance Num a => Num (Rep a) where
  (+) = liftQ2 (+)
  fromInteger = pure . fromInteger

liftQ2 :: (a -> a -> a) -> Rep a -> Rep a -> Rep a
liftQ2 op (Rep a) (Rep b) = Rep (Q.zipWith op leftA leftB <> rightA <> rightB)
  where
    l = min (Q.length a) (Q.length b)
    (leftA,rightA) = Q.splitAt l a
    (leftB,rightB) = Q.splitAt l b

get :: Rep a -> a
get (Rep q) = h where h :< _ = Q.viewl q

eShift :: Rep R -> Rep R
eShift (Rep q) = Rep (m |> h) where h :< m = Q.viewl q
