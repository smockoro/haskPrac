import Control.Monad
import Control.Applicative  

main = do
  [x,y] <- (fmap (read :: String -> Int) . words) <$> getLine
  if x > y
      then putStrLn $ show(x)
      else putStrLn $ show(y)
