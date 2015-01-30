import Control.Applicative

main = do 
    [n] <- fmap ( read :: String -> Int ) . words <$> getLine
    -- 10000 * 1/N * N(N+1)/2 = 5000 * (N+1) 
    putStrLn $ show(5000 * (n + 1) )


