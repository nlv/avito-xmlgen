{-# LANGUAGE  DeriveGeneric, OverloadedStrings, FlexibleInstances, RecordWildCards, TupleSections, Arrows #-}

module Generator
    ( generateXML
    , Config(..)
    ) where

import GHC.Generics

import Control.Applicative
import Control.Monad (mzero, guard)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List as L
import Data.List.Utils (replace)
import Data.Csv
import qualified Data.Map as Map
import qualified Data.Vector as V

import Control.Monad.Reader

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
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow

data Config = Config {
    confSrc        :: Text
  , confSkip       :: Int
  , shortUrl       :: Bool
}


generateXML :: ReaderT Config IO (Either String String)
generateXML = do
  src <- asks confSrc
  skip <- asks confSkip
  shortUrl <- asks shortUrl
  case makeGoogleExportCSVURI src of
    Nothing -> pure $ Left "Не верный URL"
    Just src' -> do
      csvData' <- lift $ openURIWithOpts [CurlFollowLocation True] $ src'
      case csvData' of
        Left err -> pure $ Left err
        Right csvData ->
          let l = encodeUtf8 $ T.unlines $ L.drop skip $ T.lines $ decodeUtf8 csvData in
          case decode NoHeader (BL.fromStrict l) of
            Left err -> pure $ Left err
            Right v -> do
              let v' = L.map V.toList $ V.toList v
              if checkId v' then do
                let v'' = mkMap v'
                [res] <- lift $ runXIOState (initialState $ Map.empty)  $ root [] [makeAds shortUrl v''] >>> writeDocumentToString [withRemoveWS yes]
                pure $ Right res
              else 
                pure $ Left "В файле не найдена колонка Id. Не верно указали сколько строк пропустить надо?"

  where  mkMap :: [[BS.ByteString]] -> [[(BS.ByteString, BS.ByteString)]]
         mkMap (h : rs) = L.map ((L.filter (\(h',_) -> h' /= "")) . L.zip h) rs
         mkMap _        = []


         checkId (h:_) = "Id" `elem` h
         checkId _     = False

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

makeAd :: Bool -> [(BS.ByteString, BS.ByteString)] -> IOStateArrow (Map.Map String String) XmlTree XmlTree
makeAd shortUrl vs = (mkelem "Ad" [] $ L.map ((makeEl shortUrl) . (dr *** dr)) $ groupAddress $ L.map (dc *** dc) vs)
  where dr = L.dropWhileEnd isSpace . L.dropWhile isSpace
        dc = T.unpack . decodeUtf8

makeEl :: Bool -> (String, String) -> IOStateArrow (Map.Map String String) XmlTree XmlTree
makeEl shortUrl (n, v)
  | n == "Description" = mkelem "Description" [] [constA ( descriptionHtml v) >>> mkCdata]

  | n == "ImageNames"  = 
      mkelem "Images" [] $ L.map (makeImage shortUrl) $ L.take 10 $ Split.splitOn ","  v    
  
  | n `L.elem` optionElems && v /= "" = mkelem n [] $ L.map (\i -> mkelem "Option" [] [ txt i]) $ Split.splitOn "|" v

  | otherwise          = mkelem n [] [ txt v]
  where
    makeImage :: Bool -> String -> IOStateArrow (Map.Map String String) XmlTree XmlTree
    makeImage False i = mkelem "Image" [ sattr "url" i] []      
    makeImage True i = 
      proc k -> do
        cache <- getUserState -< ()
        case Map.lookup i cache of
          Just i' -> (\s -> mkelem "Image" [ sattr "url" s] []) $< returnA -< i'
          Nothing -> do
            i'' <- arrIO0 (short i) -< ()
            setUserState -< Map.insert i i'' cache
            (\s -> mkelem "Image" [ sattr "url" s] []) $< returnA -< i''

    short i 
      | L.isPrefixOf "https://clck.ru/" i = pure i
      | otherwise = (either (const i) BS8.unpack) <$> (openURIWithOpts [CurlFollowLocation True] $ "https://clck.ru/-" ++ "-?url=" ++ i)
    descriptionHtml d = either (const "Ошибка в описании") id $ runPure (descriptionHtml2 d)
    descriptionHtml2 d = do
      d1 <- readMarkdown def (pack d)
      d2 <- writeHtml5String def d1
      return $ unpack d2
    optionElems = ["WorkTypes", "TeamSize", "ContactDays", "WorkDays"]

groupAddress :: [(String, String)] -> [(String, String)]
groupAddress = uncurry (:) . ((("Address",) . L.intercalate ", " . sortOn o . L.filter (/= "") . (L.map snd)) *** id) .  L.partition ((`elem` addrElems) . fst)
  where addrElems = ["addrRegion", "addrArea", "addrCity", "addrPoint", "addrStreet", "addrHouse"]
        o "addrRegion" = 1
        o "addrArea"   = 2
        o "addrCity"   = 3
        o "addrPoint"  = 4
        o "addrStreet" = 5
        o "addrHouse"  = 6
        o _            = 7

makeAds :: Bool -> [[(BS.ByteString, BS.ByteString)]] -> IOStateArrow (Map.Map String String) XmlTree XmlTree
makeAds shortUrl as
    = mkelem "Ads" [ sattr "formatVersion" "3", sattr "target" "Avito.ru" ] (L.map (makeAd shortUrl) as)
