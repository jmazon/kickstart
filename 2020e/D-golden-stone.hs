{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Control.Monad.Writer.Strict
import Data.Array.Unboxed
import Data.Array.ST
import Data.Coerce
import Data.Graph

big :: Int
big = 1000000000000

newtype Stone = Stone Int deriving (Eq,Ord,Ix)
newtype Junction = J Int deriving (Eq,Ord,Ix,Enum)

readStreet :: String -> (Junction,Junction)
readStreet s = (J a,J b) where [a,b] = map read (words s)

readStones :: String -> [Stone]
readStones = map (Stone . read) . tail . words

readRecipe :: String -> (Stone,[Stone])
readRecipe s = (last ss,init ss) where ss = readStones s

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [n,m,s,r] <- map read . words <$> getLine
    uvs <- replicateM m (readStreet <$> getLine)
    sources <- zip [J 1..] <$> replicateM n (readStones <$> getLine)
    recipes <- replicateM r (readRecipe <$> getLine)

    let sp = fw (J n) uvs
        dp = solve (J n) (Stone s) sp sources (ts (Stone s) recipes)
        ans = minimum [ dp!(j,Stone 1) | j <- [J 1..J n] ]

    putStrLn $ "Case #" ++ show i ++ ": " ++ if ans < big then show ans else "-1"

fw :: Junction -> [(Junction,Junction)] -> UArray (Junction,Junction) Int
fw n es = runSTUArray $ do
  let bds = ((J 1,J 1),(n,n))
  sp <- newArray bds big
  forM_ [J 1..n] $ \i -> writeArray sp (i,i) 0
  forM_ es $ \(u,v) -> writeArray sp (u,v) 1 *> writeArray sp (v,u) 1
  forM_ [J 1..n] $ \j -> forM_ [J 1..n] $ \i -> forM_ [J 1..n] $ \k -> do
    direct <- readArray sp (i,k)
    indirect <- (+) <$> readArray sp (i,j) <*> readArray sp (j,k)
    when (indirect < direct) $ writeArray sp (i,k) indirect
  pure sp
  
solve :: Junction -> Stone
      -> UArray (Junction,Junction) Int
      -> [(Junction,[Stone])]
      -> [(Stone,[Stone])]
      -> UArray (Junction,Stone) Int
solve n s sp sources recipes = runSTUArray $ do
  dp <- newArray ((J 1,Stone 1),(n,s)) big

  forM_ sources $ \(sourceJunction,sourceStones) ->
    forM_ [J 1..n] $ \j ->
    forM_ sourceStones $ \sourceStone ->
    writeArray dp (j,sourceStone)
      . min (sp!(sourceJunction,j)) =<< readArray dp (j,sourceStone) 

  let apply (created,ingredients) = forM_ [J 1..n] $ \rj -> do
        cost <- sum <$> forM ingredients (\i -> lift $ readArray dp (rj,i))
        forM_ [J 1..n] $ \j -> do
          let cost' = cost + sp!(rj,j)
          prev <- lift $ readArray dp (j,created)
          when (cost' < prev) $ do
            tell (Any True)
            lift $ writeArray dp (j,created) cost'

  fix $ \loop -> do
    Any moved <- execWriterT (mapM_ apply recipes)
    when moved loop

  pure dp

ts :: Stone -> [(Stone,[Stone])] -> [(Stone,[Stone])]
ts s recipes = concatMap (\gn -> [ (gn,is) | is <- rs ! gn ]) (Stone <$> topSort g)
  where
    rs = accumArray (flip (:)) [] (Stone 1,s) recipes :: Array Stone [[Stone]]
    g = buildG (1,coerce s) $
        concatMap (\(Stone gn,is) -> [ (i,gn) | Stone i <- is ] )
        recipes
