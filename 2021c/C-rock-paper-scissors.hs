{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Function
import Data.List
import Data.Monoid
import Data.Ratio
import Data.STRef
import System.Environment
import System.Exit
import System.IO
import System.Random
import Text.Printf

type Exp = Rational
data Play = R | P | S deriving (Enum,Show,Read,Ord,Eq)
type Game = [Play]

dp :: Int -> Int -> Game
dp w0 e0 = runST $ do
  let w = fromIntegral w0
      e = fromIntegral e0
  dpa <- newArray_ ((0,0,0),(60,60,60)) :: ST s (STArray s (Int,Int,Int) (Sum Exp,Game))
  writeArray dpa (0,0,0) (error "Zero has no value",error "Zero has no path")
  writeArray dpa (1,0,0) (Sum $ (e+w) / 3,[R])
  writeArray dpa (0,1,0) (Sum $ (e+w) / 3,[P])
  writeArray dpa (0,0,1) (Sum $ (e+w) / 3,[S])
  v <- newSTRef (Sum (-1),error "No best game")
  forM_ [2..60] $ \n -> do
    forM_ [0..n] $ \r -> do
      forM_ [0..n-r] $ \p -> do
        let s = n-r-p
        let vr = guard (r > 0) *> pure (((Sum ((w * fromIntegral p + e * fromIntegral (s-1)) / fromIntegral (n-1)),[R]) <>) <$> readArray dpa (r-1,p,s))
        let vp = guard (p > 0) *> pure (((Sum ((w * fromIntegral s + e * fromIntegral (r-1)) / fromIntegral (n-1)),[P]) <>) <$> readArray dpa (r,p-1,s))
        let vs = guard (s > 0) *> pure (((Sum ((w * fromIntegral r + e * fromIntegral (p-1)) / fromIntegral (n-1)),[S]) <>) <$> readArray dpa (r,p,s-1))
        m <- maximum <$> sequence (vr++vp++vs)
        writeArray dpa (r,p,s) m
        modifySTRef' v (max m)
  (_,rs) <- readSTRef v
  pure (reverse rs)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  getArgs >>= \case
    [] -> do
      t <- readLn :: IO Int
      void getLine -- x
      forM_ [1..t] $ \i -> do
        [w,e] <- map read . words <$> getLine
        putStrLn $ "Case #" ++ show i ++ ": " ++ concatMap show (dp w e)
    [x0] -> judge x0

evaluate :: (Int,Int,String) -> Exp
evaluate (w,e,ans) = play w e (map (read . pure) g)
  where [_,_,g] = words ans

play :: Int -> Int -> Game -> Exp
play w0 e0 = sum . snd . mapAccumL f (0,0,0)
  where
    w = fromIntegral w0
    e = fromIntegral e0
    f (0,0,0) pl = (t,(w+e)%3)
      where t = case pl of R -> (1,0,0)
                           P -> (0,1,0)
                           S -> (0,0,1)
    f (r,p,s) pl = case pl of
      R -> ((r+1,p,s),(w*p + e*s)%n :: Rational)
      P -> ((r,p+1,s),(w*s + e*r)%n)
      S -> ((r,p,s+1),(w*r + e*p)%n)
      where n = r + p + s

judge :: String -> IO ()
judge x0 = do
  let x = fromIntegral (read x0 :: Integer)

  gs <- replicateM 50 (randomRIO (5,95))
  let wes0 = concatMap (\g -> let w = 10*g in [(w,w),(w,w `div` 2),(w,w `div` 10),(w,0)]) gs
  print (length wes0)
  putStrLn x0

  results <- flip fix wes0 $ \loop wes -> if null wes then pure [] else do
    i <- randomRIO (0,length wes-1)
    let (pre,(w,e):post) = splitAt i wes
    putStrLn $ show w ++ " " ++ show e
    ans <- getLine
    hPutChar stderr '.'
    ((w,e,ans) :) <$> loop (post++pre)
  hPutChar stderr '\n'
  hClose stdin
  hClose stdout

  let score = sum (map evaluate results)
  hPrint stderr $ (fromRational (score/200) :: Double)
  if (score < 200*x)
    then do
      hPutStrLn stderr (printf "Expected bonus too low (avg <%s)" x0)
      exitFailure
    else do hPutStrLn stderr "Success"
