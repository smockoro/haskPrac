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
    t <- (read :: String -> Int ) <$> getLine
    n <- (read :: String -> Int ) <$> getLine
    as <- fmap ( read :: String -> Int ) . words <$> getLine
    m <- ( read :: String -> Int ) <$> getLine
    bs <- fmap ( read :: String -> Int ) . words <$> getLine
    checkPerchase t as bs

checkPerchase t as bs
    | bs == [] = putStrLn "yes"
    | as == [] && (length bs) > 0 = putStrLn "no"
    | b `elem` [a .. (a + t)] =  checkPerchase t (tail as) (tail bs) 
    | not ( b `elem` [a .. (a + t)] ) = checkPerchase t (tail as) bs
    where a = head as 
          b = head bs

