import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as B

readInt :: B.ByteString -> Int
readInt = fst . fromMaybe (error "Unreadable Int") . B.readInt

main :: IO ()
main = do
  t <- readInt <$> B.getLine :: IO Int
  forM_ [1..t] $ \i -> do
    n <- readInt <$> B.getLine
    css <- replicateM n (map readInt . B.words <$> B.getLine)
    let answer = maximum $ foldl1 (\as bs -> zipWith (+) ([0] ++ as) (bs ++ repeat 0)) css
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (answer :: Int)
