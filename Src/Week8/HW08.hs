module HW08 where

import System.Random
import Control.Monad.Random
import Data.Monoid
import Data.List
import Control.Monad 
-- import Control.Monad.Random.Class

{-
Exercise 1 failed on me:

import Data.List
import Data.Char
import Data.Maybe


stringFitsFormat :: String -> Bool
stringFitsFormat = isJust . go
  where go :: String -> Maybe String
        go [] = Nothing
        go (x:xs)
          | isLetter x = Nothing
          | otherwise = let l = length $
                                takeWhile isLetter (dropWhile isDigit (x:xs))
                        in if l == (digitToInt x) then Just "" else Nothing
-}


specialNumbers :: [Int]
specialNumbers = [ x | x <- [1..100],  mod x 5  == 0, mod x 7 /= 0 ]

type StdRand = Rand StdGen

type Army = Int

type DieRoll = Int

data ArmyCounts = ArmyCounts { attackers :: Army, defenders :: Army } deriving Show


instance Monoid ArmyCounts where
  mempty = ArmyCounts 0 0
  ArmyCounts fAttack fDef `mappend` ArmyCounts sAttack sDef =
    ArmyCounts (fAttack + sAttack) (fDef + sDef)


dieRoll :: StdRand DieRoll
dieRoll = getRandomR (1,6)

dec :: DieRoll -> DieRoll -> ArmyCounts
dec a d
  | a > d = ArmyCounts 0 (-1)
  | otherwise = ArmyCounts (-1) 0

battleResults :: [DieRoll] -> [DieRoll] -> ArmyCounts
battleResults [] [] = mempty
battleResults _ [] = mempty
battleResults [] _ = mempty
battleResults att def =
  let rSort = reverse.sort
      (x:xs) = rSort att
      (y:ys) = rSort def
  in dec x y `mappend` battleResults xs ys


--  Exercise 4

{-
failed battle

battle :: ArmyCounts -> ArmyCounts
battle (ArmyCounts att def) =
  let mapEval = map evalRandIO
      attL = mapEval $ replicate att dieRoll
      defL = mapEval $ replicate def dieRoll
  in (battleResults attL defL) `mappend` ArmyCounts att def
-}


battle :: ArmyCounts -> StdRand ArmyCounts
battle (ArmyCounts att def) =
  let attList = tr $ dice $ min att 3
      defList = tr $ dice $ min def 2
  in  liftM2 battleResults attList defList

invade :: ArmyCounts -> StdRand ArmyCounts
invade (ArmyCounts att def)
  | def > 0 = 

dice :: Int -> [StdRand DieRoll]
dice n = replicate n dieRoll

tr :: [StdRand DieRoll] -> StdRand [DieRoll]
tr = sequence


extractRoll :: [StdRand DieRoll] -> IO [DieRoll]
extractRoll = sequence . map evalRandIO

bR :: IO [DieRoll] -> IO [DieRoll] -> IO ArmyCounts
bR fstt secc = do
  f <- fstt
  s <- secc
  let x = battleResults f s
  return x

ch :: IO [DieRoll] -> IO [DieRoll] -> IO ArmyCounts
ch fstt secc =  secc >>= (\y -> fstt >>= (return . battleResults y))

battleF :: ArmyCounts -> IO ArmyCounts
battleF (ArmyCounts att def) =
  let z = extractRoll . dice
  in ch (z att) (z def)


