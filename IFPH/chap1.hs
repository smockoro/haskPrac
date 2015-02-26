-- vim: set fileencoding=utf-8 :
--
--       Author:   Takahiro Oshima <tarotora51@gmail.com>
--       License:  MIT License
--       Created:  2015-02-25
--

import Control.Applicative
import Control.Monad

-- 4.3.6
filter' p = concat . map box
            where box x = if p x then [x] else []

-- 4.3.7
takeWhile' p (x:xs) = if p x then x : takeWhile' p xs else []

dropWhile' p (x:xs) = if p x then dropWhile' p xs else (x:xs)

-- 4.3.9
pairs n = [(x,y) | x <- [1 .. n], y <- [x .. n]]

-- 4.3.10
fourSquare n = [(a,b,c,d) | (a,b,c,d) <- tetras n, (squrs (a,b,c,d) || squrs (a,c,b,d) || squrs (a,d,b,c))]
squrs (a,b,c,d) = (a ^ 2 + b ^ 2 == c ^ 2 + d ^ 2) 
tetras n = [(a,b,c,d) | a <- [1 .. n], b <- [(a+1) .. n], c <- [(b+1) .. n], d <- [(c+1) .. n]]
