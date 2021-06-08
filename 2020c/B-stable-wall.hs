{-# LANGUAGE RecordWildCards #-}
import Control.Monad
import Data.Array
import Data.Char
import Data.Graph
import Data.Maybe
import Data.Tree
import qualified Data.Set as Set

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \tn -> do
    [r,c] <- map read . words <$> getLine
    wall <- listArray ((1,1),(r,c)) . concat . reverse <$> replicateM r getLine
    let ps = Set.fromList (elems wall)
    let g = buildG (ord 'A',ord 'Z')
            [ (ord a,ord b)
            | (i,j) <- indices wall, i > 1
            , let a = wall!(i,j), let b = wall!(i-1,j) ]
        s = filter ((`Set.member` ps) . rootLabel) $
            map (fmap chr) (scc g)
        answer = mapM (\Node{..} -> guard (null subForest) *> pure rootLabel) s
    putStrLn $ "Case #" ++ show tn ++ ": " ++ fromMaybe "-1" answer
