-- vim: set fileencoding=utf-8 :
--
--       Author:   Takahiro Oshima <tarotora51@gmail.com>
--       License:  MIT License
--       Created:  2015-01-30
--
module <+FILE NAME+> where 

-- | テスト内容を記述
--
-- >>> function
-- result
--

import Control.Applicative
import Control.Monad
import Data.List

main = do 
    [n] <- fmap ( read :: String -> Int ) . words <$> getLine
    putStrLn $ show(2 * n)
