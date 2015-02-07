-- vim: set fileencoding=utf-8 :
--
--       Author:   Takahiro Oshima <tarotora51@gmail.com>
--       License:  MIT License
--       Created:  2015-02-06
--

import Control.Applicative
import Control.Monad
import Data.List
import Text.Printf

main = do 
    [n,m] <- fmap ( read :: String -> Int ) . words <$> getLine 
    solve n m

solve n m
    | even m = check n m (m `div` 2) 0 0
    | odd m = check n m ((m - 3) `div` 2) 1 0

check n m a g b
    | n == (a + g + b) && m == (2 * a + 3 * g + 4 * b) = printf "%d %d %d \n" a g b
    | a >= 3 = check n m (a - 3) (g + 2) b
    | a >= 2 = check n m (a - 2) g (b + 1)
    | a <= 1 && g >= 4 = check n m a (g - 4) (b + 3)
    | a == 1 && g == 2 = check n m (a - 1) (g - 2) (b + 2)
    | otherwise = putStrLn "-1 -1 -1"
    -- | otherwise = putStrLn $ show([a,g,b])
    -- | a >= 3 && (n - (a + g + b)) > 1 = check n (a - 3) (g + 2) b
