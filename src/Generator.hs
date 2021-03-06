{-# LANGUAGE  DeriveGeneric, OverloadedStrings, FlexibleInstances, RecordWildCards #-}

module Generator
    ( generateXML
    ) where

import GHC.Generics

import Control.Applicative
import Control.Monad (mzero, guard)
import qualified Data.ByteString.Lazy as BL 
import Data.List as L
import Data.List.Utils (replace)
import Data.Csv
import qualified Data.Vector as V



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
import qualified Text.XML.HXT.DOM.XmlNode as Node

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
             , adImages :: ![String]
             } deriving Generic


instance FromNamedRecord Ad where
  parseNamedRecord r = 
      Ad <$> r .: "Id" 
         <*> (trimString <$> (r .: "DateBegin"  <|> pure ""))
         <*> (trimString <$> (r .: "DateEnd"  <|> pure ""))
         <*> (r .: "AdStatus" <|> pure "") 
         <*> (r .: "AllowEmail" <|> pure "")
         <*> (r .: "ManagerName" <|> pure "") 
         <*> r .: "ContactPhone" 
         <*> ((L.intercalate ", ") <$> L.filter (not . L.null) <$> sequenceA (L.map (r .:) address))
         <*> r .: "Category" 
         <*> (r .: "Condition" <|> pure "") 
         <*> r .: "GoodsType" 
         <*> r .: "GoodsSubType" 
         <*> r .: "AdType" 
         <*> r .: "Title" 
         <*> (fmap descriptionHtml (r .: "Description"))
         <*> r .: "Price" 
         <*> r .: "VideoURL" 
         <*> ((Split.splitOn ",") <$> r .:  "ImageNames")
      where address = ["addrRegion", "addrArea", "addrCity", "addrPoint", "addrStreet", "addrHouse"]
            -- descriptionHtml d = either (unpack . renderError) id $ runPure (descriptionHtml2 d)
            descriptionHtml d = either (const "???????????? ?? ????????????????") id $ runPure (descriptionHtml2 d)
            descriptionHtml2 d = do
              d1 <- readMarkdown def (pack d)
              d2 <- writeHtml5String def d1
              return $ unpack d2            

trimString = L.dropWhileEnd isSpace . L.dropWhile isSpace
 

generateXML :: Text -> IO (Either String String)
generateXML src = 
  case makeGoogleExportCSVURI src of
    Nothing -> pure $ Left "???? ???????????? URL"
    Just src' -> do
      csvData' <- openURIWithOpts [CurlFollowLocation True] $ src'
      case csvData' of
        Left err -> pure $ Left err
        Right csvData ->
          let l = encodeUtf8 $ T.unlines $ L.drop 0 $ T.lines $ decodeUtf8 csvData in
          case decodeByName (BL.fromStrict l) of
            Left err -> pure $ Left err
            Right (_, v) -> do
              [res] <- runX $ root [] [makeAds (V.toList v)] >>> writeDocumentToString [withRemoveWS yes]
              pure $ Right res
              -- pure $ Right $ decodeUtf8 $ BL.toStrict res
              -- [res] <- runX $ root [] [makeAds (V.toList v)] >>> writeDocument [withIndent yes] "Ads.xml"
              -- pure $ Right $ Node.toText res

makeGoogleExportCSVURI :: Text -> Maybe String
makeGoogleExportCSVURI x = maybe Nothing (Just . renderStr) ((mkURI x) >>= convertURI)
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
makeAd ad@(Ad {adGoodsType = "????????????????", ..}) = 
        mkelem "Ad" []
          [ mkelem "Id" [] [ txt (adId) ]
          , mkelem "DateBegin" [] [ txt (adDateBegin) ]
          , mkelem "DateEnd" [] [ txt (adDateEnd) ]
          , mkelem "Status" [] [ txt (adStatus) ]
          , mkelem "AllowEmail" [] [ txt (adAllowEmail) ]
          , mkelem "ManagerName" [] [ txt (adManagerName) ]
          , mkelem "ContactPhone" [] [ txt (adContactPhone) ]
          , mkelem "Address" [] [ txt (adAddress) ]
          , mkelem "Category" [] [ txt (adGoodsType ad) ]
          , mkelem "Condition" [] [ txt (adCondition) ]
          , mkelem "Industry" [] [ txt (adGoodsSubType) ]
          , mkelem "AdType" [] [ txt (adType) ]
          , mkelem "Title" [] [ txt (adTitle) ]
          , mkelem "Description" [] [ constA (adDescription) >>> mkCdata ]
          , mkelem "Salary" [] [ txt (adPrice) ]
          , mkelem "VideoURL" [] [ txt (adVideoURL) ]
          , mkelem "Images" [] $ images (adImages)
          ]
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
          , mkelem "AdType" [] [ txt (adType ad) ]
          , mkelem "Title" [] [ txt (adTitle ad) ]
          , mkelem "Description" [] [ constA (adDescription ad) >>> mkCdata ]
          , mkelem "Price" [] [ txt (adPrice ad) ]
          , mkelem "VideoURL" [] [ txt (adVideoURL ad) ]
          , mkelem "Images" [] $ images (adImages ad)
          ]

images :: ArrowXml a => [String] -> [a XmlTree XmlTree]
images is = L.map (\i -> mkelem "Image" [ sattr "url" i ] []) $ (L.take 8) is

makeAds :: ArrowXml a => [Ad] -> a XmlTree XmlTree
makeAds as
    = mkelem "Ads" [ sattr "formatVersion" "3", sattr "target" "Avito.ru" ] (L.map makeAd as)
