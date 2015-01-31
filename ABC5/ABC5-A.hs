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
    [x,y] <- fmap ( read :: String -> Int ) . words <$> getLine
    putStrLn $ show(y `div` x)
