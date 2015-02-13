-- vim: set fileencoding=utf-8 :
--
--       Author:   Takahiro Oshima <tarotora51@gmail.com>
--       License:  MIT License
--       Created:  2015-02-12
--

import Control.Applicative
import Control.Monad
import Data.List
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

-- スタック
type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
        push 3
        pop 
        pop
 
-- キュー
type Queue = [Int]

dequeue :: State Queue Int
dequeue = state $ \(x:xs) -> (x, xs)
                         
enqueue :: Int -> State Queue ()
enqueue a = state $ \xs -> ((), xs ++ [a])

queueManip :: State Queue Int 
queueManip = do
        dequeue
        enqueue 2
        enqueue 4
        dequeue

-- プライオリティキュー
--type Priority = Int
--type PQElem = (Int, Priority)

--enpriqueue :: PQElem -> State Queue ()
--enpriqueue a = state $ \xs -> ((), xs ++ [a])

--depriqueue :: State Queue PQElem
--depriqueue = state $ \


-- キュー with Writerモナド
dequeue' :: Writer [String] (State Queue Int)
dequeue' = return( state $ \(x:xs) -> (x, xs) )

enqueue' :: Int -> Writer [String] (State Queue ())
enqueue' a = do
        tell ["Enqueue " ++ show a]
        return( state $ \xs -> ((), xs ++ [a]) )

queueManip' :: Writer [String] (State Queue Int)
queueManip' = do
        dequeue'
        enqueue' 2
        enqueue' 4
        dequeue'

--　畳み込みながらログを取る
logfold xs acc
    | xs == [] = return(acc)
    | otherwise = do
        tell ["plus " ++ show(head xs)]
        logfold (tail xs) (acc + (head xs))

pluslogfold acc x = do
        tell [" + " ++ show x]
        return(acc + x)
--msc :: Ord a => [a] -> Int
msc xs = maximum [scount z zs | z:zs <- tails' xs]
scount x xs = length $ fst $ runWriter (filterM (comp' x) xs)
scountlog x xs = mapM_ putStrLn $ snd  $ runWriter (filterM (comp' x) xs)

tails' [] = []
tails' (x:xs) = (x:xs):tails' xs 

comp' x y 
    | x >= y = do
        tell [ show x ++ " is greater than or equal to " ++ show y ]
        return False
    | x < y = do
        tell [ show x ++ " is less than " ++ show y ]
        return True

table xs = [(z, scount z zs) | z:zs <- tails' xs]

