module HW07 where

import System.Random

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2  =  0:1:zipWith (+) fibs2 (tail fibs2)
                

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

countFrom :: Integer -> Stream Integer
countFrom n = n `Cons` countFrom (n+1)

listToStream :: [a] -> Stream a
listToStream (x:xs) = Cons x (listToStream xs)

instance Show a => Show (Stream a) where
  show stream = let (Cons x xs) = listToStream $ take 20 $streamToList stream
                in  show x ++ " `Cons` " ++ show xs

streamRepeat :: a -> Stream a
streamRepeat x = x `Cons` streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = (f x) `Cons` streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = (f x) `Cons` streamFromSeed f x

nats :: Stream Integer
nats = countFrom 0

-- ruler :: Stream Integer
-- This function is bloody boring so I'll skip it.

randomList :: (Random a, RandomGen g) => g -> [a]
randomList = randoms 

randomInts :: Int -> [Int]
randomInts int = randoms (mkStdGen int)

-- getStdRanddom :: (StdGen -> (a, StdGen)) -> IO a
-- getStdRanddom :: IO (a, StdGen)
-- getStdRanddom = getStdGen >>= return . random

f :: (Random a) => StdGen -> (a, StdGen)
f = random


getStdRanddom :: (StdGen -> (a, StdGen)) -> IO a
getStdRanddom x = getStdGen >>= return . x >>= (\(aa, _) -> return aa)
