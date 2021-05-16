{-# LANGUAGE  DeriveGeneric, OverloadedStrings, FlexibleInstances #-}

module Lib
    ( someFunc
    ) where

import GHC.Generics

import Control.Applicative
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as BL 
import Data.List as L
import Data.List.Utils (replace)
import Data.Csv
import qualified Data.Vector as V

import Text.Pandoc

import Data.Text
import Data.Set as S
import Data.Default

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
--             , adImagesDir :: !String
--             , adImagesCnt :: Int
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
         <*> ((L.intercalate ", ") <$> L.filter (not . L.null) <$> sequenceA (L.map (r .:) address))
         <*> r .: "category" 
         <*> r .: "condition" 
         <*> r .: "goodsType" 
         <*> r .: "goodsSubType" 
         <*> r .: "type" 
         <*> r .: "title" 
         <*> (fmap descriptionHtml (r .: "description"))
         <*> r .: "price" 
         <*> r .: "videoURL" 
--                <*> r .: "images"
      where address = ["addrRegion", "addrArea", "addrCity", "addrPoint", "addrStreet", "addrHouse"]
            -- descriptionHtml d = either (unpack . renderError) id $ runPure (descriptionHtml2 d)
            descriptionHtml d = either (const "Ошибка в описании") id $ runPure (descriptionHtml2 d)
            descriptionHtml2 d = do
              d1 <- readMarkdown def (pack d)
              d2 <- writeHtml5String def d1
              return $ unpack d2            


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
          , mkelem "Status" [] [ txt (adStatus ad) ]
          , mkelem "AllowEmail" [] [ txt (adAllowEmail ad) ]
          , mkelem "ManagerName" [] [ txt (adManagerName ad) ]
          , mkelem "ContactPhone" [] [ txt (adContactPhone ad) ]
          , mkelem "Address" [] [ txt (adAddress ad) ]
          , mkelem "Category" [] [ txt (adCategory ad) ]
          , mkelem "Condition" [] [ txt (adCondition ad) ]
          , mkelem "GoodsType" [] [ txt (adGoodsType ad) ]
          , mkelem "GoodsSubType" [] [ txt (adGoodsSubType ad) ]
          , mkelem "Type" [] [ txt (adType ad) ]
          , mkelem "Title" [] [ txt (adTitle ad) ]
          , mkelem "Description" [] [ constA (adDescription ad) >>> mkCdata ]
          , mkelem "Price" [] [ txt (adPrice ad) ]
          , mkelem "VideoURL" [] [ txt (adVideoURL ad) ]
          , mkelem "Images" [] $ images (adTitle ad)
          ]
   where images :: ArrowXml a => String -> [a XmlTree XmlTree]
         images title = L.map (\i -> mkelem "Image" [ sattr "name" i ] []) (imagesSet title)

imagesSet "Металлочерепица, металосайдинг, профнастил" = 
  [ "https://sun9-67.userapi.com/impg/LsMFd-8xdiKyWCSKY2wzeD0hIbK4ixzRH2we0g/l3x4ZHv2_3U.jpg?size=1365x960&quality=96&sign=e24146baaf99fb811e1c16442600c828"
  , "https://sun9-48.userapi.com/impg/lsNWVZWMaplSkJdr-N1k8T0weuABYM7DLLcZHQ/Q-Q2E8nAW_I.jpg?size=853x853&quality=96&sign=576342fb0a9b6c430b13e5f9f0b4cd49"
  , "https://sun9-54.userapi.com/impg/PJMBMEyMpAezfkTm2cB0_jfQz48ckH24e3n5ig/oaHS7MvqFPc.jpg?size=960x960&quality=96&sign=f8edf18be857e5effbf1a389b0725c03"
  , "https://sun9-18.userapi.com/impg/zLdtmOLmfFesYn0SyCduMUNbv1OlyxzRsySVRw/ctAHU9cOB74.jpg?size=720x900&quality=96&sign=5e6225402d4b5e00ecdbba7ad80c5957"
  , "https://sun9-24.userapi.com/impg/4u1osQlGLbOXrn_RtR-qMcabem6XUGb7_yhH2A/iMAfajKVHTE.jpg?size=1080x565&quality=96&sign=ddecf8bebc489f34f84cf94ba5741ee9"
  , "https://sun9-36.userapi.com/impg/kNA1niwQyUStB0nnlgfGtcnplfZ9OEWsJx20Wg/8bU0dD3-cYY.jpg?size=1080x565&quality=96&sign=955f1f3d8599c46aa35f1cf920d1b1a0"
  , "https://sun9-71.userapi.com/impg/Lh0AYguvzae4IQkQdZhMYmOrTI3GMgRJwcG1lQ/Gj2Zpags06A.jpg?size=1080x565&quality=96&sign=fb6cc961bda10ac5c6e49d9e3d0ec585"
  ] 
imagesSet "Пластиковые окна пвх с доставкой" = 
  [ "https://sun9-24.userapi.com/impg/2EYPPg-TConjzm_TxAx4wGDmVFbtdPJAd37Khg/fJRc9UxzFHI.jpg?size=1000x750&quality=96&sign=cf1acb36ac0b2f05ffde8979b2043e34"
  , "https://sun9-19.userapi.com/impg/ij7rkj3K8wM_GTUnxHB93vQkphKfImBu-87mOQ/cpp26g4J-mo.jpg?size=1280x852&quality=96&sign=3fae060d84ee29bfa4dee940cb477234"
  , "https://sun9-71.userapi.com/impg/5bekEiAbKatfVP4tOKXv4Bk7NxN6QdpdVPhWkw/qlHsIe0m85g.jpg?size=1280x720&quality=96&sign=655554e56a49ebd0ed5da94b044fb76c"
  ]
imagesSet _ = []

makeAds :: ArrowXml a => [Ad] -> a XmlTree XmlTree
makeAds as
    = mkelem "Ads" [ sattr "formatVersion" "3", sattr "target" "Avito.ru" ] (L.map makeAd as)
