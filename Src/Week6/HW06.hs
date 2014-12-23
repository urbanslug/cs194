{-
  Name: Njagi Mwaniki
  Resources:
    Learn You A Haskell: Monoid, Functors, Applicative Functors and Monads
    Aeson tutorial      - FPcomplete
    Aeson Documentation - Hackage
-}

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HW06 where

import Data.Aeson
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text                  as T
-- import qualified Data.Text.IO               as T


import Data.List

inputFile :: FilePath
inputFile = "Src/Week6/markets.json"

outputFile :: FilePath
outputFile = "Src/Week6/outMart.json"

ynToBool :: Value -> Value
ynToBool (Object o)   = Object (fmap ynToBool o)
ynToBool (Array a)    = Array (fmap ynToBool a)
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool v            = v

parseData :: B.ByteString -> Either String Value
parseData byteStringData = fmap ynToBool $ eitherDecode byteStringData

data Market = Market { marketname :: T.Text
                     ,  x         :: Float
                     ,  y         :: Float
                     ,  state     :: T.Text
                     } deriving (Show, Eq, Generic)
-- Thanks to deriving Generic we get our type becoming automatic instances of
-- FromJSON and ToJSON and therefore get automatic parsers for this.
-- e.g eitherDecode, decode, encode
 
instance FromJSON Market

instance ToJSON Market

nullMarket :: Market
nullMarket = Market {marketname ="", x=0, y=0, state=""}

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets byteStringData = fmap (removeResult . fromJSON) $ parseData byteStringData

-- To get a list of Markets from a Result.
-- I think a more elegant way would be `eitherDecode byteStringData`

removeResult :: Result [Market] -> [Market]
removeResult (Success a) = a
removeResult (Error s) = [Market {marketname = "Failed Result parsing"
                                 , x = 0
                                 , y = 0
                                 , state = (T.pack s)} ]

loadData :: IO [Market]
loadData = do
  file <- B.readFile inputFile
  return $ getMarket $ parseMarkets file

-- Gets [Market] from Either String [Market]
getMarket :: Either String [Market] -> [Market]
getMarket (Right r) = r
getMarket (Left l) = fail l

data OrdList a = OrdList { getOrdList :: [a] }
               deriving (Eq, Show)

instance Ord Market where
  compare (Market _ x1 _ _) (Market _ x2 _ _) = compare x1 x2

instance Ord a => Monoid (OrdList a) where
  mempty  =  OrdList []
  mappend = (\(OrdList xs) (OrdList ys) ->  OrdList (xs ++ ys))
  mconcat = foldr mappend mempty

type Searcher m =  T.Text -> [Market] -> m

search :: Monoid m => (Market -> m) -> Searcher m
search _ _ [] = mempty
search func text lst = mconcat (map func (fmap (matchName text) lst))

makeMonoid :: Market -> OrdList Market
makeMonoid mar
 | mar == nullMarket = mempty
 | otherwise         = OrdList [mar]

matchName :: T.Text -> Market -> Market
matchName text market =
  (\c ->
    case (T.isInfixOf c (marketname market)) of True  -> market
                                                False -> nullMarket) text

firstFound :: Searcher (Maybe Market)
firstFound searchWord list =
  let martList = allFound searchWord list
  in case martList of [] -> Nothing
                      _  -> Just $ head martList

lastFound :: Searcher (Maybe Market)
lastFound searchWord list =
  let martList = allFound searchWord list
  in case martList of [] -> Nothing
                      _  -> Just $ last martList

allFound :: Searcher [Market]
allFound searchWord list = getOrdList $ search makeMonoid searchWord list

numberFound :: Searcher Int
numberFound searchWord list = length $ allFound searchWord list

orderedNtoS :: Searcher [Market]
orderedNtoS searchWord list =  sort $ allFound searchWord list

{-

-- Alternative of matchName which gives Text. Is much better.
f :: Market -> T.Text -> T.Text
f market text =
  (\a ->
    case (T.isInfixOf a (marketname market)) of True -> a
                                                False -> None) text

-- Test passed.
testLoadData :: IO ()
testLoadData =(fmap encode loadData) >>=  B.writeFile outputFile

-- Typechecks
parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets byteStringData = eitherDecode byteStringData

-- To help testParseData get Value from eitherDecode so as to pass it to encode.
getValueFromEither :: Either String Value -> Value
getValueFromEither (Right a) = a
getValueFromEither (Left b) = String (T.pack b)

-- Test passed
testParseData :: IO ()
testParseData = do
  file <- B.readFile inputFile
  B.writeFile outputFile (encode . getValueFromEither $ parseData file)

-- Test passed
testYnToBool :: IO ()
testYnToBool = do
  file <- B.readFile inputFile
  B.writeFile outputFile (encode . ynToBool . fromJust $ decode file)

-}
