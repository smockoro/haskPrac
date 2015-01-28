main = getLine >>= putStrLn . solve

solve :: String -> String
solve = filter (/= 'a') . filter (/= 'i') . filter (/= 'u') . filter (/= 'e') . filter (/= 'o' ) 
