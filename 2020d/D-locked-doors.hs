{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.Monad
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Data.Maybe
import Data.Semigroup
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

type Room = Int
type Door = Int
type Difficulty = Int

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [n,q] <- map read . words <$> getLine
    ds <- V.fromListN (n-1) . map read . words <$> getLine
    let ps = cartesianBasins ds
        qt = mkBinLiftTree ps
    rs <- replicateM q $ do
      [s,k] <- map read . words <$> getLine
      pure $ query ds qt s k
    putStrLn $ "Case #" ++ show i ++ ": " ++ unwords (show <$> rs)

interestingWith :: Ord b => (a -> b) -> [a] -> a -> ([a],Maybe a)
interestingWith f s v =
  case dropWhile ((< f v) . f) s of
    [] -> ([v],Nothing)
    s'@(h:_) -> (v:s',Just h)

cartesianBasins :: Vector Difficulty -> Vector (Maybe Door)
cartesianBasins v = parent where
  interestingLeft = vMapAccumL (interestingWith snd) [] (V.indexed v)
  interestingRight = vMapAccumR (interestingWith snd) [] (V.indexed v)

  parent :: Vector (Maybe Door)
  parent = V.zipWith cb interestingLeft interestingRight

  cb :: Maybe (Door,Difficulty) -> Maybe (Door,Difficulty) -> Maybe Door
  cb l r = getArg . getMin <$> m
    where m = (Min . uncurry (flip Arg) <$> l)
              <> (Min . uncurry (flip Arg) <$> r)

data Tree = Bin
  { treeLeft  :: !(Maybe Tree)
  , treeDoor  :: !Door
  , treeRight :: !(Maybe Tree)
  , treeSize  :: Int
  }
  deriving Show

data Trees = Trees
  { treesNodes :: Vector Tree
  , treesBinLift :: Vector [Door]
  }
  deriving Show

mkBinLiftTree :: Vector (Maybe Door) -> Trees
mkBinLiftTree ps = Trees ts bl where
  ts = V.accumulate addChild (V.imap (\i _ -> Bin Nothing i Nothing (nodeSize i)) ps)
                             (V.imapMaybe parentToChild ps)

  nodeSize :: Door -> Int
  nodeSize i = let Bin l _ r _ = ts V.! i
               in maybe 0 treeSize l + 1 + maybe 0 treeSize r

  addChild :: Tree -> Either Tree Tree -> Tree
  addChild (Bin Nothing n r s) (Left x) = Bin (Just x) n r s
  addChild (Bin l n Nothing s) (Right x) = Bin l n (Just x) s
  addChild _ _ = error "addChild: resulting tree isn't binary as expected"

  parentToChild :: Door -> Maybe Door -> Maybe (Door,Either Tree Tree)
  parentToChild i mbP = mbP >>= \p -> pure (if i < p then (p,Left (ts V.! i)) else (p,Right (ts V.! i)))

  bl = V.map (catMaybes . V.toList) $ vTranspose $ V.unfoldr parents ps

  parents :: Vector (Maybe Door) -> Maybe (Vector (Maybe Door),Vector (Maybe Door))
  parents p | V.all isNothing p = Nothing
            | otherwise = Just (p,n)
    where n = V.map (>>= (p V.!)) p

binLift :: (Tree -> Bool) -> Trees -> Door -> Tree
binLift p Trees{..} = go where
  go i | p t = t
       | null ups = treesNodes V.! head ups'
       | otherwise = go (last ups)
    where t = treesNodes V.! i
          (ups,ups') = break (p . (treesNodes V.!)) (treesBinLift V.! i)

query :: Vector Difficulty -> Trees -> Room -> Int -> Room
query ds ts@Trees{..} s k
  | treeSize (treesNodes V.! firstDoor) >= k-1 =
      if firstDoor == leftDoor then s - k + 1
                              else s + k - 1
  | basin <- binLift ((>= k-1) . treeSize) ts firstDoor =
      if firstDoor < treeDoor basin
      then treeDoor basin + k - maybe 0 treeSize (treeLeft basin)
      else treeDoor basin + 2 - (k - 1 - maybe 0 treeSize (treeRight basin))
  where Just (Min (Arg _ firstDoor)) =
                 (Min . flip Arg leftDoor <$> ds V.!? leftDoor) <>
                 (Min . flip Arg rightDoor <$> ds V.!? rightDoor)
        leftDoor = s-2
        rightDoor = s-1
    
------------------------------------------------------------
-- General utility

getArg :: Arg a b -> b
getArg (Arg _ x) = x

vMapAccumL :: (a -> b -> (a,c)) -> a -> Vector b -> Vector c
vMapAccumL f a0 v = evalState (V.mapM go v) a0
  where
    go b = state (\s -> let (s',c) = f s b in (c,s'))

vMapAccumR :: (a -> b -> (a,c)) -> a -> Vector b -> Vector c
vMapAccumR f a0 v = V.create $ do
  let n = V.length v
  c <- MV.new n
  void $ flip runStateT a0 $ V.forM_ (V.enumFromStepN (n-1) (-1) n) $ \i -> do
    x <- V.indexM v i
    x' <- state (\s -> let (s',x') = f s x in (x',s'))
    MV.write c i x'
  pure c

vTranspose :: Vector (Vector a) -> Vector (Vector a)
vTranspose vv = V.generate n $ \i -> V.generate m $ \j -> vv V.! j V.! i
  where n = V.length (V.head vv)
        m = V.length vv
