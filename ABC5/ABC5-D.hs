-- vim: set fileencoding=utf-8 :
--
--       Author:   Takahiro Oshima <tarotora51@gmail.com>
--       License:  MIT License
--       Created:  2015-01-31
--

import Control.Applicative
import Control.Monad
import Data.List

main = do 
    n <- (read :: String -> Int) <$> getLine
    ds <- replicateM n getLine
    q <- (read :: String -> Int) <$> getLine
    ps <- replicateM q readLn

    --ds' <- sort $ concat $ map (read :: String -> Int) <$> fmap words ds

    solve ps ( reverse $ sort $ concat $ map (read :: String -> Int) <$> fmap words ds ) []

solve ps ds as
    | ps /= [] = solve (tail ps) ds (as ++ [sum $ take (head ps) ds])
    | ps == [] = mapM_ print as


