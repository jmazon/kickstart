import Control.Monad

main :: IO ()
main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    g <- readLn :: IO Int

    putStrLn $ "Case #" ++ show i ++ ": " ++ show (length
      [ ()
      | i <- [1..ceiling (sqrt (2 * fromIntegral g))]
      , let (q,r) = (2*g) `divMod` i, r == 0
      , let (k,r') = (q - i + 1) `divMod` 2, r' == 0
      , k > 0 ])

    -- k + k+1 + k+2 + ... + k+i-1 = i × (k + k+i-1) / 2 = g
    -- 2g / i = 2k + i - 1
