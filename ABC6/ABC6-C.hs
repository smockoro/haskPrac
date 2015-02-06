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
    | even m = check n (m `div` 2) 0 0
    | odd m = check n ((m - 3) `div` 2) 1 0

check n a g b
    | n == (a + g + b) = printf "%d %d %d \n" a g b
    | a <= 1 = putStrLn "-1 -1 -1"
    | a >= 5 && ((a + g + b) - n) > 1 = check n (a - 5) (g + 2) (b + 1)
    | a >= 2 && ((a + g + b) - n) > 1 = check n (a - 2) g (b + 1)
    -- | a >= 3 && (n - (a + g + b)) > 1 = check n (a - 3) (g + 2) b
