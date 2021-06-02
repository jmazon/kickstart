import           Control.Monad (forM_,replicateM,filterM)
import           Data.List     (maximumBy,mapAccumL)
import           Data.Maybe    (fromMaybe)
import           Data.Ord      (comparing,Down(Down))
import qualified Data.Map.Strict as Map

type Time = Int
type Toy = (Time,Time)
type Result = (Int,Maybe Time)

readToy :: String -> Toy
readToy s = (a,b) where [a,b] = map read (words s)

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    n <- readLn
    toys <- replicateM n (readToy <$> getLine)
    let (removed,time) | n <= 12 = brute toys
                       | otherwise = solve toys
    putStrLn $ "Case #" ++ show i ++ ": " ++ show removed
      ++ " " ++ maybe "INDEFINITELY" show time

brute :: [Toy] -> Result
brute toys0 = best $ map eval $ filterM (const [False,True]) $ zip toys0 [0 :: Int ..]
  where
    eval toys = (length toys0 - length toys,go Map.empty 0 (toys ++ toys)) where
      go _ 0 [] = Just 0
      go _ _ [] = Nothing
      go m t (((e,r),i):ts)
        | Just t0 <- Map.lookup i m, t - t0 < r = Just t
        | otherwise = go (Map.insert i (t+e) m) (t+e) ts

best :: [Result] -> Result
best = maximumBy (comparing (fromMaybe maxBound . snd) <> comparing (Down . fst))

solve :: [Toy] -> Result
solve toys0 = checkIndet $ mapAccumL go (Map.empty,t0,t0,0) (zip toys0 [0..])
  where
    t0 = sum (fst <$> toys0)

    go (toys,lapE,totalE,removed) ((e,r),i) =
        ( prune (Map.insert (e+r,i) e toys) lapE (totalE + e) removed
        , (removed,Just totalE)
        )

    prune toys' le te rm = case Map.maxViewWithKey toys' of
      Just (((er,_),e'),toys'') | er > le -> prune toys'' (le-e') (te-2*e') (rm+1)
      _ -> (toys',le,te,rm)

    checkIndet ((_,_,totalE,removed),r)
      | totalE > 0 = (removed,Nothing)
      | otherwise = best r
