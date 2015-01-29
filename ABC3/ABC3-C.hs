mport Control.Applicative
import Data.List

main = do   
  [n,k] <- fmap ( read :: String -> Int ) . words <$> getLine
  rs <- fmap ( read :: String -> Float ) . words <$> getLine
  culcRate n k 1 (sort rs) 0.0

culcRate n k count rs c
    | count <= (n - k) = culcRate n k (count + 1) (tail rs) 0.0
    | count > (n - k) && count <= n = culcRate n k (count + 1) (tail rs) ( (c + (head rs)) / 2.0 )
    | count > n = putStrLn $ show(c)
