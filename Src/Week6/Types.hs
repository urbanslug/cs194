{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Map
import Control.Applicative 
import Control.Monad (mzero)
import qualified Data.Aeson as J
import Data.Aeson ((.:))
import qualified Data.ByteString.Lazy as T

data Market = Market {
    mktFmid::Int,
    mktMarketname::String,
    mktWebsite::String,
    mktStreet::String,
    mktCity::String,
    mktCounty::String,
    mktState::String,
    mktZip::String,
    mktSeason1date::String,
    mktSeason1time::String,
    mktSeason2date::String,
    mktSeason2time::String,
    mktSeason3date::String,
    mktSeason3time::String,
    mktSeason4date::String,
    mktSeason4time::String,
    mktX::Int,
    mktY::Int,
    mktLocation::String,
    mktCredit::String,
    mktWic::String,
    mktWiccash::String,
    mktSfmnp::String,
    mktSnap::String,
    mktBakedgoods::String,
    mktCheese::String,
    mktCrafts::String,
    mktFlowers::String,
    mktEggs::String,
    mktSeafood::String,
    mktHerbs::String,
    mktVegetables::String,
    mktHoney::String,
    mktJams::String,
    mktMaple::String,
    mktMeat::String,
    mktNursery::String,
    mktNuts::String,
    mktPlants::String,
    mktPoultry::String,
    mktPrepared::String,
    mktSoap::String,
    mktTrees::String,
    mktWine::String,
    mktUpdatetime::String
} deriving (Show)

instance J.FromJSON Market where
    parseJSON (J.Object v) = 
        Market <$>
        v .: "fmid" <*>
        v .: "marketname" <*>
        v .: "website" <*>
        v .: "street" <*>
        v .: "city" <*>
        v .: "county" <*>
        v .: "state" <*>
        v .: "zip" <*>
        v .: "season1date" <*>
        v .: "season1time" <*>
        v .: "season2date" <*>
        v .: "season2time" <*>
        v .: "season3date" <*>
        v .: "season3time" <*>
        v .: "season4date" <*>
        v .: "season4time" <*>
        v .: "x" <*>
        v .: "y" <*>
        v .: "location" <*>
        v .: "credit" <*>
        v .: "wic" <*>
        v .: "wiccash" <*>
        v .: "sfmnp" <*>
        v .: "snap" <*>
        v .: "bakedgoods" <*>
        v .: "cheese" <*>
        v .: "crafts" <*>
        v .: "flowers" <*>
        v .: "eggs" <*>
        v .: "seafood" <*>
        v .: "herbs" <*>
        v .: "vegetables" <*>
        v .: "honey" <*>
        v .: "jams" <*>
        v .: "maple" <*>
        v .: "meat" <*>
        v .: "nursery" <*>
        v .: "nuts" <*>
        v .: "plants" <*>
        v .: "poultry" <*>
        v .: "prepared" <*>
        v .: "soap" <*>
        v .: "trees" <*>
        v .: "wine" <*>
        v .: "updatetime" 

    parseJSON _ = mzero


decodeMarkets :: T.ByteString -> Maybe [Market] 
decodeMarkets = J.decode 

printMarkets :: FilePath -> IO (Maybe [Market])
printMarkets fp = T.readFile fp >>= return . decodeMarkets
