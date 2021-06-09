import           Control.Monad   (forM_,replicateM)
import           Data.List       (foldl')
import           Data.Map.Strict (Map)
import           Data.Maybe      (fromMaybe)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    [n,k] <- map read . words <$> getLine
    strings <- replicateM n getLine
    let trie = foldl' trieInsert emptyTrie strings
        V{vScore=answer} = foldTrie (solve k) trie
    putStrLn $ "Case #" ++ show i ++ ": " ++ show answer

data V = V { vTips :: !Int, vScore :: !Int } deriving Show

solve :: Int -> Int -> Int -> [V] -> V
solve k d n vs = V
  { vTips = m
  , vScore = d*q + sum (vScore <$> vs)
  }
  where (q,m) = (n + sum (vTips <$> vs)) `divMod` k

data Trie = Node !Int !(Map Char Trie) deriving Show

emptyTrie :: Trie
emptyTrie = Node 0 Map.empty

trieInsert :: Trie -> String -> Trie
trieInsert (Node n t) [] = Node (n+1) t
trieInsert (Node n t) (c:s) = Node n (Map.alter ins c t)
  where ins mbT = Just $ trieInsert (fromMaybe emptyTrie mbT) s

foldTrie :: (Int -> Int -> [a] -> a) -> Trie -> a
foldTrie f = go 0 where
  go d (Node n t) = f d n (map (go $! d+1) (Map.elems t))
