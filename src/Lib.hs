{-# LANGUAGE  DeriveGeneric, OverloadedStrings, FlexibleInstances #-}

module Lib
    ( someFunc
    ) where

import GHC.Generics

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
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
         <*> ( (++) <$> (r .: "addrCity") <*> (r .: "addrStreet"))
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

ad1 = Ad { adId = "Sayding_001"
         , adDateBegin = "16.05.2021 10:00"
         , adDateEnd = "18.05.2021 10:00"
         , adStatus = "Free"
         , adAllowEmail = "Да"
         , adManagerName = "Илья"
         , adContactPhone = "8 929 301-42-00"
         , adAddress = "Омская область, Омск, ул. Осминина, 16"
         , adCategory = "Ремонт и строительство"
         , adCondition = "Новое"
         , adGoodsType = "Стройматериалы"
         , adGoodsSubType = "Отделка"
         , adType = "Товар приобретен на продажу"
         , adTitle = "Сайдинг"
         , adDescription = description
         , adPrice = "450"
         , adVideoURL = "http://www.youtube.com/watch?v=YKmDXNrDdBI" 
         , adImagesDir = "http://img.test.ru/8F7B-4A4F3A0F2BA1.JPG"
         }
  where description = " \
          \ <p>Описание:</p> \
          \ <ul> \
          \ <li>Сайдинг металлический, виниловый! \
          \ <li>Работаем без посредников! \
          \ <li>Свой цех! \
          \ <li>Монтаж в короткие сроки! \
          \ <li>Качество гарантируем! \
          \ <li>Скидки всем! \
          \ <li>Рассрочка до 20 мес, без банка! \
          \ <li>Замер и консультация специалиста бесплатно! \
          \ </ul>"


someFunc :: IO ()
someFunc = do
  csvData <- BL.readFile "ads.csv"
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, v) -> do
      runX $ root [] [makeAds (V.toList v)] >>> writeDocument [withIndent yes] "Ads.xml"
      return ()
{-
    Right (_, v) -> V.forM_ v $ \ p ->
      putStrLn $ adAddress p
-}

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
