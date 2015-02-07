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
    n <- readLn
    putStrLn $ show(n - 1)
