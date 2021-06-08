{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Monad      (forM_,replicateM)
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Set           (Set)
import qualified Data.Set as Set

type Pos = (Int,Int)

readPair :: String -> (Int,Int)
readPair s = (x,y) where [x,y] = map read (words s)

main :: IO ()
main = do
  t <- readLn:: IO Int
  forM_ [1..t] $ \i -> do
    [s,ra,pa,rb,pb,c] <- map read . words <$> getLine
    crps <- replicateM c (readPair <$> getLine)
    let answer = solve s (ra,pa) (rb,pb) crps
    putStrLn $ "Case #" ++ show i ++ ": " ++ show answer

data S = S
  { pA, pB :: !(Maybe Pos)
  , pScore :: !Int
  , pCl :: !(Set Pos)
  }

solve :: Int -> Pos -> Pos -> [Pos] -> Int
solve s a b cs = negaMax (transition s) start
  where start = S
          { pA = Just a
          , pB = Just b
          , pScore = 0
          , pCl = Set.insert b (Set.insert a (Set.fromList cs))
          }

isDone :: S -> Bool
isDone S{pA=Nothing,pB=Nothing} = True
isDone _ = False

transition :: Int -> S -> NonEmpty S
transition s st@S{pA = Just p,pScore,pCl} =
  case [ st { pA = Just p', pScore = pScore + 1, pCl = Set.insert p' pCl }
       | p' <- neighbors p, inRange s p', p' `Set.notMember` pCl
       ]
  of []  -> st { pA = Nothing } :| []
     h:t -> h :| t
transition _ st@S{pA=Nothing} = st :| []

inverse :: S -> S
inverse S{..} = S
  { pA = pB
  , pB = pA
  , pScore = negate pScore
  , pCl
  }

neighbors :: Pos -> [Pos]
neighbors (p,r)
  | even r = [(p-1,r-1),(p,r-1),(p,r+1)]
  | odd  r = [(p,r-1),(p,r+1),(p+1,r+1)]

inRange :: Int -> Pos -> Bool
inRange s (p,r) =
  min p r >= 1
  && p <= s
  && r <= 2*p-1

negaMax :: (S -> NonEmpty S) -> S -> Int
negaMax _ st | isDone st = pScore st
negaMax tr st = maximum (fmap (negate . negaMax tr . inverse) (tr st))
