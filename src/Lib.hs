{-# LANGUAGE  DeriveGeneric, OverloadedStrings, FlexibleInstances #-}

module Lib
    ( someFunc
    ) where

import GHC.Generics

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.List as L
import Data.List.Utils (replace)
import Data.Csv
import qualified Data.Vector as V

import Data.List.Split

import Text.XML.HXT.Core

data Ad = Ad { adId :: !String
             , adDateBegin :: !String
             , adDateEnd :: !String
             , adStatus :: !String
             , adAllowEmail :: !String
             , adManagerName :: !String
             , adContactPhone :: !String
             , adAddress :: !String
             , adCategory :: !String
             , adCondition :: !String
             , adGoodsType :: !String
             , adGoodsSubType :: !String
             , adType :: !String
             , adTitle :: !String
             , adDescription :: !String
             , adPrice :: !String
             , adVideoURL :: !String
             , adImagesDir :: !String
             } deriving Generic


instance FromNamedRecord Ad where
  parseNamedRecord r = 
      Ad <$> r .: "id" 
         <*> r .: "dateBegin" 
         <*> r .: "dateEnd" 
         <*> r .: "status" 
         <*> r .: "allowEmail" 
         <*> r .: "managerName" 
         <*> r .: "contactPhone" 
         <*> fmap (replace ", ," ",") (r .: "address")
         -- <*> fmap (L.intercalate ", ") ((:) <$> (r.: "addrStreet") <*> fmap (: []) (r .: "addrHouse"))
         <*> r .: "category" 
         <*> r .: "condition" 
         <*> r .: "goodsType" 
         <*> r .: "goodsSubType" 
         <*> r .: "type" 
         <*> r .: "title" 
         <*> r .: "description" 
         <*> r .: "price" 
         <*> r .: "videoURL" 
         <*> r .: "images"


someFunc :: IO ()
someFunc = do
  csvData <- BL.readFile "ads.csv"
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, v) -> do
      runX $ root [] [makeAds (V.toList v)] >>> writeDocument [withIndent yes] "Ads.xml"
      return ()

makeAd :: ArrowXml a => Ad -> a XmlTree XmlTree
makeAd ad = 
        mkelem "Ad" []
          [ mkelem "Id" [] [ txt (adId ad) ]
          , mkelem "DateBegin" [] [ txt (adDateBegin ad) ]
          , mkelem "DateEnd" [] [ txt (adDateEnd ad) ]
          , mkelem "AdStatus" [] [ txt (adStatus ad) ]
          , mkelem "AllowEmail" [] [ txt (adAllowEmail ad) ]
          , mkelem "ManagerName" [] [ txt (adManagerName ad) ]
          , mkelem "ContactPhone" [] [ txt (adContactPhone ad) ]
          , mkelem "Address" [] [ txt (adAddress ad) ]
          , mkelem "Category" [] [ txt (adCategory ad) ]
          , mkelem "Condition" [] [ txt (adCondition ad) ]
          , mkelem "GoodsType" [] [ txt (adGoodsType ad) ]
          , mkelem "GoodsSubType" [] [ txt (adGoodsSubType ad) ]
          , mkelem "AdType" [] [ txt (adType ad) ]
          , mkelem "Title" [] [ txt (adTitle ad) ]
          , mkelem "Description" [] [ constA (adDescription ad) >>> mkCdata ]
          , mkelem "Price" [] [ txt (adPrice ad) ]
          , mkelem "VideoURL" [] [ txt (adVideoURL ad) ]
          , mkelem "Images" [] $ images [adImagesDir ad]
          ]
   where images :: ArrowXml a => [String] -> [a XmlTree XmlTree]
         images ix = map (\i -> mkelem "Image" [ sattr "url" i ] []) ix

makeAds :: ArrowXml a => [Ad] -> a XmlTree XmlTree
makeAds as
    = mkelem "Ads" [ sattr "formatVersion" "3", sattr "target" "Avito.ru" ] (map makeAd as)
