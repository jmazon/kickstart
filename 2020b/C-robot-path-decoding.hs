import Control.Monad (forM_)
import Data.Char (digitToInt)
import Data.List (foldl1')
import Text.Parsec

m :: Int
m = 10 ^ (9 :: Int)

data Pos = !Int :+ !Int deriving Show

add :: Pos -> Pos -> Pos
add (a :+ b) (c :+ d) = ((a + c) `mod` m) :+ ((b + d) `mod` m)

scale :: Int -> Pos -> Pos
scale f (a :+ b) = (f * a `mod` m) :+ (f * b `mod` m)

program :: Parsec String () Pos
program = foldl1' add <$> many1 move where
  move = card <|> rep
  card =
    ( ((-1) :+   0 ) <$ char 'N' ) <|>
    ( (  1  :+   0 ) <$ char 'S' ) <|>
    ( (  0  :+   1 ) <$ char 'E' ) <|>
    ( (  0  :+ (-1)) <$ char 'W' ) 
  rep = scale . digitToInt <$> digit <*> between (char '(') (char ')') program

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    Right (h :+ w) <- fmap (add (0:+0)) . parse (program <* eof) "stdin" <$> getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (w+1) ++ " " ++ show (h+1)
