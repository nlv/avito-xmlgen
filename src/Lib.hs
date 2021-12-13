{-# LANGUAGE  DeriveGeneric, OverloadedStrings, FlexibleInstances, RecordWildCards #-}

module Lib
    ( someFunc
    ) where

import GHC.Generics

import Control.Applicative
import Control.Monad (mzero, guard)
import qualified Data.ByteString.Lazy as BL 
import Data.List as L
import Data.List.Utils (replace)
import Data.Csv
import qualified Data.Vector as V

import Options.Applicative

import Text.Pandoc
import Network.Curl.Download
import Network.Curl.Opts

import Text.URI

import Data.Char (isSpace)

import Data.Text as T
import Data.Text.Encoding
import Data.Set as S 
import Data.Default
import Data.List.NonEmpty as NEL

import Data.List.Split as Split

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
            --  , adCondition :: !String
            --  , adGoodsType :: !String
            --  , adGoodsSubType :: !String
            --  , adType :: !String
             , adServiceType :: !String
             , adServiceSubType :: !String            
             , adTitle :: !String
             , adDescription :: !String
             , adPrice :: !String
             , adVideoURL :: !String
             , adImages :: ![String]
             } deriving Generic


instance FromNamedRecord Ad where
  parseNamedRecord r = 
      Ad <$> r .: "Id" 
         <*> (trimString <$> (r .: "DateBegin"))
         <*> (trimString <$> (r .: "DateEnd"))
         <*> r .: "AdStatus" 
         <*> r .: "AllowEmail" 
         <*> r .: "ManagerName" 
         <*> r .: "ContactPhone" 
         <*> ((L.intercalate ", ") <$> L.filter (not . L.null) <$> sequenceA (L.map (r .:) address))
         <*> r .: "Category" 
        --  <*> (r .: "Condition")
        --  <*> r .: "GoodsType" 
        --  <*> r .: "GoodsSubType" 
        --  <*> r .: "AdType" 
         <*> r .: "ServiceType" 
         <*> r .: "ServiceSubType"         
         <*> r .: "Title" 
         <*> (fmap descriptionHtml (r .: "Description"))
         <*> r .: "Price" 
         <*> r .: "VideoURL" 
         <*> ((Split.splitOn ",") <$> r .:  "ImageNames")
      where address = ["addrRegion", "addrArea", "addrCity", "addrPoint", "addrStreet", "addrHouse"]
            -- descriptionHtml d = either (unpack . renderError) id $ runPure (descriptionHtml2 d)
            descriptionHtml d = either (const "Ошибка в описании") id $ runPure (descriptionHtml2 d)
            descriptionHtml2 d = do
              d1 <- readMarkdown def (pack d)
              d2 <- writeHtml5String def d1
              return $ unpack d2            

trimString = L.dropWhileEnd isSpace . L.dropWhile isSpace


someFunc :: IO ()
someFunc = 
  let ops = argument str (help "URL of data") in do
  src <- execParser $ info (ops <**> helper) (fullDesc <> progDesc "Converting Avito posts" <> Options.Applicative.header "header")
  case makeGoogleExportCSVURI src of
    Nothing -> putStrLn "Не верный URL"
    Just src' -> do
      putStrLn "Hello"
      putStrLn $ "src = " ++ src'
      csvData' <- openURIWithOpts [CurlFollowLocation True] $ src'
      case csvData' of
        Left err -> putStrLn $ "Error: " ++ err
        Right csvData ->
          let l = encodeUtf8 $ T.unlines $ L.drop 3 $ T.lines $ decodeUtf8 csvData in
          case decodeByName (BL.fromStrict l) of
            Left err -> putStrLn err
            Right (_, v) -> do
              runX $ root [] [makeAds (V.toList v)] >>> writeDocument [withIndent yes] "Ads.xml"
              return ()

makeGoogleExportCSVURI :: String -> Maybe String
makeGoogleExportCSVURI x = maybe Nothing (Just . renderStr) ((mkURI $ T.pack x) >>= convertURI)
   where convertURI URI { uriPath = Nothing, ..} = Nothing
         convertURI u@(URI { uriPath = (Just (s, p)), uriQuery = qs, uriFragment = frag, ..}) = do
           p' <- sequence $ NEL.filter (\i -> i /= mkPathPiece "edit") (NEL.map Just p) 
           exportPath <- mkPathPiece "export"
           formatParam <- QueryParam <$> mkQueryKey "format" <*> mkQueryValue "csv"
           gidHash <- (T.splitOn "=") . unRText <$> frag
           guard (L.length gidHash == 2)
           guard (L.head gidHash == "gid")
           gidParam <- QueryParam <$> mkQueryKey "gid" <*> mkQueryValue (L.head $ L.tail gidHash)
           let p'' = p' ++ [exportPath]
           p''' <- if L.null p'' then Nothing else Just $ u {uriPath = Just (s, (L.head p'') NEL.:| L.tail p''),
                                                             uriQuery = formatParam : gidParam : qs, uriFragment = Nothing}
           return p'''

makeAd :: ArrowXml a => Ad -> a XmlTree XmlTree
-- makeAd ad@(Ad {adGoodsType = "Вакансии", ..}) = 
--         mkelem "Ad" []
--           [ mkelem "Id" [] [ txt (adId) ]
--           , mkelem "DateBegin" [] [ txt (adDateBegin) ]
--           , mkelem "DateEnd" [] [ txt (adDateEnd) ]
--           , mkelem "Status" [] [ txt (adStatus) ]
--           , mkelem "AllowEmail" [] [ txt (adAllowEmail) ]
--           , mkelem "ManagerName" [] [ txt (adManagerName) ]
--           , mkelem "ContactPhone" [] [ txt (adContactPhone) ]
--           , mkelem "Address" [] [ txt (adAddress) ]
--           , mkelem "Category" [] [ txt (adGoodsType ad) ]
--           -- , mkelem "Condition" [] [ txt (adCondition) ]
--           , mkelem "Industry" [] [ txt (adGoodsSubType) ]
--           , mkelem "JobType" [] [ txt (adType) ]
--           , mkelem "Title" [] [ txt (adTitle) ]
--           , mkelem "Description" [] [ constA (adDescription) >>> mkCdata ]
--           , mkelem "Salary" [] [ txt (adPrice) ]
--           , mkelem "VideoURL" [] [ txt (adVideoURL) ]
--           , mkelem "Images" [] $ images (adImages)
--           ]
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
          -- , mkelem "Condition" [] [ txt (adCondition ad) ]
          -- , mkelem "GoodsType" [] [ txt (adGoodsType ad) ]
          -- , mkelem "GoodsSubType" [] [ txt (adGoodsSubType ad) ]
          -- , mkelem "Type" [] [ txt (adType ad) ]
          , mkelem "ServiceType" [] [ txt (adServiceType ad) ]
          , mkelem "ServiceSubType" [] [ txt (adServiceSubType ad) ]          
          , mkelem "Title" [] [ txt (adTitle ad) ]
          , mkelem "Description" [] [ constA (adDescription ad) >>> mkCdata ]
          , mkelem "Price" [] [ txt (adPrice ad) ]
          , mkelem "VideoURL" [] [ txt (adVideoURL ad) ]
          , mkelem "Images" [] $ images (adImages ad)
          ]

images :: ArrowXml a => [String] -> [a XmlTree XmlTree]
images is = L.map (\i -> mkelem "Image" [ sattr "url" i ] []) is

makeAds :: ArrowXml a => [Ad] -> a XmlTree XmlTree
makeAds as
    = mkelem "Ads" [ sattr "formatVersion" "3", sattr "target" "Avito.ru" ] (L.map makeAd as)
