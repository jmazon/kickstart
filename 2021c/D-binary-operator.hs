{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative hiding ((<|>))
import Control.DeepSeq (force)
import Control.Monad (forM_,replicateM)
import Control.Monad.State (runState,evalState,MonadState,get,modify,put)
import Data.Functor (($>),void)
import Data.IORef
import System.Random
import System.Timeout
import Text.Parsec
import qualified Data.Map as M

data Expr =
    Lit Integer
  | Plus Expr Expr
  | Times Expr Expr
  | Op Expr Expr
  deriving (Eq,Ord,Show)

main :: IO ()
main = do
  t <- readLn
  forM_ [1..t] $ \i -> do
    n <- readLn
    Right exprs <- mapM (parse expr "<stdin>") <$> replicateM n getLine
    answer <- classify (10000000 `div` t) exprs
    putStrLn $ "Case #" ++ show i ++ ": " ++ unwords (show <$> answer)

assign :: (Ord a,MonadState (M.Map a Int) m) => a -> m Int
assign e = do
  c <- get
  case M.lookup e c of
    Just i -> pure i
    Nothing -> do
      let i = M.size c + 1
      modify (M.insert e i)
      pure i

type Parser = Parsec String ()
expr,num,binop :: Parser Expr
expr = num <|> between (char '(') (char ')') binop
num = Lit . read <$> many1 digit
binop = do
  a <- expr
  o <- op
  b <- expr
  pure (o a b)
op :: Parser (Expr -> Expr -> Expr)
op = char '+' $> Plus
  <|> char '*' $> Times
  <|> char '#' $> Op

eval :: MonadState (M.Map (Integer,Integer) Integer,StdGen) m => Expr -> m Integer
eval (Lit l) = pure l
eval (Plus a b) = liftA2 (+) (eval a) (eval b)
eval (Times a b) = liftA2 (*) (eval a) (eval b)
eval (Op a b) = do
  x <- eval a
  y <- eval b
  (vs,rnd) <- get
  case M.lookup (x,y) vs of
    Just v -> pure v
    Nothing -> do
      let (v,rnd') = random rnd
      put (M.insert (x,y) v vs,rnd')
      pure v

classify :: Int -> [Expr] -> IO [Int]
classify to es = do
  tableau <- newIORef (map (const []) es)
  let go rnd = do
        let (vs,(_,rnd')) = runState (mapM eval es) (M.empty,rnd)
        modifyIORef' tableau (force . zipWith (:) vs)
        go rnd'
  void $ timeout to (go =<< getStdGen)
  vss <- readIORef tableau
  pure $ evalState (mapM assign vss) M.empty
