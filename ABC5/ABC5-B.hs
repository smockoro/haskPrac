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
    --[n] <- fmap ( read :: String -> Int ) . words <$> getLine
    n <- readLn
    ts <- replicateM n getLine
    putStrLn $ show $ foldl (min) 101 $ map (read :: String -> Int) ts 














