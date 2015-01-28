import Control.Applicative

main = do 
    ss <- getLine
    ts <- getLine
    comp ss ts

comp [] [] = putStrLn "You can win"
comp (s:ss) (t:ts) 
    | s == t = comp ss ts
    | s /= t && s == '@' = if t `elem` ['a','t','c','o','d','e','r'] then comp ss ts
                                        else putStrLn "You will lose"
    | s /= t && t == '@' = if s `elem` ['a','t','c','o','d','e','r'] then comp ss ts
                                        else putStrLn "You will lose"
    | s /= t = putStrLn "You will lose"
