import Control.Monad
import Data.List

solve :: (Int,Int) -> String -> (Int,Int)
solve (ks,kss) s
  | "KICK" `isPrefixOf` s = (ks+1,kss)
  | "START" `isPrefixOf` s = (ks,kss+ks)
  | otherwise = (ks,kss)

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    s <- getLine
    let answer = snd $ foldl' solve (0,0) (tails s)
    putStrLn $ "Case #" ++ show i ++ ": " ++ show answer
