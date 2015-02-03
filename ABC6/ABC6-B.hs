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
    tribonacci (n - 3) 1 0 0

tribonacci n therd second first
    | n /= 0 = tribonacci (n - 1) (therd + second + first) therd second
    | n == 0 = putStrLn $ show( therd `mod` 10007 )

