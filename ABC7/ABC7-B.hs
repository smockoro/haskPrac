-- vim: set fileencoding=utf-8 :
--
--       Author:   Takahiro Oshima <tarotora51@gmail.com>
--       License:  MIT License
--       Created:  2015-02-07
--

import Control.Applicative
import Control.Monad
import Data.List

main = do
    ss <- getLine
    solve 1 ss []

solve str_num (x:xs) ys
    | str_num == 1 && xs == [] && x == 'a' = putStrLn "-1"
    | str_num == 1 && xs == [] = putStrLn "a"
    | xs == [] = putStrLn ys
    | otherwise = solve (str_num + 1) xs (ys ++ "a")


