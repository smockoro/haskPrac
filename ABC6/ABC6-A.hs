-- vim: set fileencoding=utf-8 :
--
--       Author:   Takahiro Oshima <tarotora51@gmail.com>
--       License:  MIT License
--       Created:  2015-02-04
--

import Control.Applicative
import Control.Monad
import Data.List

main = do
    n <- readLn
    if n `mod` 3 == 0 
        then putStrLn "YES"
        else putStrLn "NO"
