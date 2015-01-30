-- vim: set fileencoding=utf-8 :
--
--       Author:   Takahiro Oshima <tarotora51@gmail.com>
--       License:  MIT License
--       Created:  2015-01-30
--

import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do 
    cs0 <- reverse . words <$> getLine
    cs1 <- reverse . words <$> getLine
    cs2 <- reverse . words <$> getLine
    cs3 <- reverse . words <$> getLine
    putLow cs3 >> putLow cs2 >> putLow cs1 >> putLow cs0

putLow (x:xs)
    | xs == [] = putStrLn (x)
    | otherwise = putStr (x ++ " ") >> putLow xs

