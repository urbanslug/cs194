module HW04 where

import Src.Week3.BST
import Data.Char
import Data.List


-- This function always returns the second argument passed to it.
-- Because the second argument is of type b.
ex1 :: a -> b -> b
ex1 first second = second


-- This function always returns the first argument passed to it.
-- Either the first or second argument can be returned and it will still typecheck because they are of the same type.
ex2 :: a -> a -> a
ex2 first second = first

-- This function always returns the second argument passed to it.
-- This is because the second argument is of type a
ex3 :: Int -> a -> a
ex3 first second = second


-- Takes 3 args but must return a type of either second or third type
-- It therefore returns the third. The second could've been chosen too.
-- Two functions second is: ex4 first second third = second
ex4 :: Bool -> a -> a -> a
ex4 first second third = third

-- This takes a bool and can return it or its negation.
-- Therefore two distict implementations.
-- ex5 bool = bool
ex5 :: Bool -> Bool
ex5 bool = not bool

-- f refuses to be partially applied.
ex6 :: (a -> a) -> a
ex6 f = let x = f x in x
-- ex6 f = f (fix f) 


-- F acts on x and the result is of type x. 
ex7 :: (a -> a) -> a -> a
ex7 f x = f x

-- Takes a list and returns the same list. 
ex8 :: [a] -> [a]
ex8 list = list

-- Same definition as fmap.
-- Takes a function and maps a list to that function.
ex9 :: (a -> b) -> [a] -> [b]
ex9 f list = map f list

-- Not finite. Only works when the Maybe a value is not nothing.
-- It fails.
ex10 :: Maybe a -> a
ex10 (Just x) = x
ex10 Nothing = error "The value passed was a Nothing"

-- Wraps a given value in a Just.
ex11 :: a -> Maybe a
ex11 x = Just x

-- This is the same as an a -> a function
-- Finite
ex12 :: Maybe a -> Maybe a
ex12 (Just x) = Just x
ex12 Nothing = Nothing

insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ a Leaf = Node Leaf a Leaf
insertBST f a (Node left node right) =
  case f a node of GT -> Node left node (insertBST f a right)
                   LT -> Node (insertBST f a left) node right
                   _ -> Node left node right

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

allCaps :: [String] -> Bool
-- allCaps []     = True
-- allCaps (x:xs) = (map toUpper x == x) && allCaps xs
allCaps = foldr (\x -> (&&) (map toUpper x == x)) True

dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = unwords.words

firstLetters :: [String] -> [Char]
-- firstLetters [] = []
-- firstLetters xs = [head x | x <- xs]
--firstLetters (x:xs) = head x : firstLetters xs
firstLetters = (intercalate "," . map show)

asList :: [String] -> String
asList = (\c -> "[" ++ c ++ "]") . intercalate "," 
