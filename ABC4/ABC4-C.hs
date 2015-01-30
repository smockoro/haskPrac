-- vim: set fileencoding=utf-8 :
--
--       Author:   Takahiro Oshima <tarotora51@gmail.com>
--       License:  MIT License
--       Created:  2015-01-30
--

import Control.Applicative
import Control.Monad
import Data.List

main = do
  [n] <- fmap ( read :: String -> Int ) . words <$> getLine
  -- カードの並びは30の倍数で同じ並びになる
  replaceCard (n `mod` 30) 0 "123456"

replaceCard n i (e1:e2:e3:e4:e5:e6:[])
    | i < n && (i `mod` 5 == 0) = replaceCard n (i + 1) (e2:e1:e3:e4:e5:e6:[])
    | i < n && (i `mod` 5 == 1) = replaceCard n (i + 1) (e1:e3:e2:e4:e5:e6:[])
    | i < n && (i `mod` 5 == 2) = replaceCard n (i + 1) (e1:e2:e4:e3:e5:e6:[])
    | i < n && (i `mod` 5 == 3) = replaceCard n (i + 1) (e1:e2:e3:e5:e4:e6:[])
    | i < n && (i `mod` 5 == 4) = replaceCard n (i + 1) (e1:e2:e3:e4:e6:e5:[])
    | i >= n = putStrLn (e1:e2:e3:e4:e5:e6:[])

