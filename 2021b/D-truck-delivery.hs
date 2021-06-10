{-# LANGUAGE RecordWildCards #-}

import Control.Monad (forM_,replicateM)
import Data.Array
import Data.Array.ST
import Data.Bifunctor (second)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [n,q] <- map read . words <$> getLine
    roads <- concat <$> replicateM (n-1) (readRoad <$> getLine)
    queries <- accumArray (flip (:)) [] (1,n) . zipWith readQuery [1..] <$>
              replicateM q getLine
    let tree = buildTree n roads
        answers = dfs q queries tree stEmpty
    putStrLn $ "Case #" ++ show i ++ ": " ++
      unwords (maybe "0" (show . getGcd) <$> elems answers)

loadLow,loadHigh :: Load
loadLow = 0
loadHigh = 2 * 10 ^ (5 :: Int)

type City = Int
type Load = Int

data Road = Road { roadToll :: !Gcd, roadLimit :: !Load } deriving Show

readRoad :: String -> [(City,(Road,City))]
readRoad s = [(x,(r,y)),(y,(r,x))] where
  [x,y,l,a] = map read (words s)
  r = Road (Gcd a) l

data Query = Query { qIndex :: !Int, qCity :: !City, qLoad :: !Load } deriving Show
readQuery :: Int -> String -> (City,Query)
readQuery i s = (c,Query i c w)
  where [c,w] = map read (words s)

data Tree = Tree
  { nodeCity  :: !City
  , nodeRoads :: ![(Road,Tree)]
  }
  deriving Show

buildTree :: Int -> [(City,(Road,City))] -> Tree
buildTree n roads = go 0 1 where
  adj :: Array City [(Road,City)]
  adj = accumArray (flip (:)) [] (1,n) roads
  go :: City -> City -> Tree
  go p i = Tree i $ map (second (go i)) (filter ((/= p) . snd) (adj ! i))

newtype Gcd = Gcd { getGcd :: Int } deriving (Eq,Ord,Show)
instance Semigroup Gcd where Gcd a <> Gcd b = Gcd (gcd a b)

dfs :: Int -> Array City [Query] -> Tree -> SegmentTree -> Array Int (Maybe Gcd)
dfs q qs t0 st0 = runSTArray $ do
  r <- newArray_ (1,q)
  let go Tree{..} st = do
        mapM_ (\Query{..} -> writeArray r qIndex $! stGet st qLoad) (qs ! nodeCity)
        mapM_ (\(Road{..},tree) -> go tree (stInsert roadLimit roadToll st)) nodeRoads
  go t0 st0
  pure r
    

data SegmentTree = St
  { stLeft :: !(Maybe SegmentTree)
  , stValue :: !(Maybe Gcd)
  , stRight :: !(Maybe SegmentTree)
  }
  deriving Show

stEmpty :: SegmentTree
stEmpty = St Nothing Nothing Nothing

stGet :: SegmentTree -> Load -> Maybe Gcd
stGet st0 x = go loadLow loadHigh st0 where
  go lo hi st | hi <= lo+1 = stValue st
              | x < m = maybe mempty (go lo m) (stLeft st)
              | otherwise = maybe mempty stValue (stLeft st) <>
                            maybe mempty (go m hi) (stRight st)
    where m = (lo + hi) `div` 2

stInsert :: Load -> Gcd -> SegmentTree -> SegmentTree
stInsert x v = go loadLow loadHigh where
  go lo hi st | hi <= lo+1 = v `seq` St Nothing (Just v) Nothing
              | x < m = let l' = go lo m (fromMaybe stEmpty (stLeft st))
                            v' = stValue l' <> maybe mempty stValue (stRight st)
                        in l' `seq` v' `seq` st { stLeft = Just l', stValue = v'}
              | otherwise = let r' = go m hi (fromMaybe stEmpty (stRight st))
                                v' = maybe mempty stValue (stLeft st) <> stValue r'
                            in r' `seq` v' `seq` st { stRight = Just r', stValue = v' }
    where m = (lo + hi) `div` 2
