{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative

import System.IO

import Data.Aeson
import Network.HTTP.Simple
import qualified Data.Conduit.List      as CL
import qualified Data.ByteString        as S

import Codec.Xlsx hiding (Parser)
import Control.Lens hiding ((.=), argument)

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import Data.Time.Clock.POSIX

data Opts = Opts {
    optFileName    :: String
  , optCount       :: Int
  , optUrl         :: String
  , optOutFileName :: String
}

optParser :: Parser Opts
optParser = Opts 
              <$> argument str (help "Filename of data")
              <*> option auto ( long "count" <> short 'c' <> help "Count of randomized cases")
              <*> option str ( long "url" <> short 'u' <> help "Url of randomizing service")
              <*> option str ( long "out" <> short 'o' <> help "Output filename")

main :: IO ()
main = run =<< execParser opts
    where opts = info (optParser <**> helper)
                    ( fullDesc
                    <> progDesc "Randomize text"
                    <> header "Randomize text")

run :: Opts -> IO ()
run opts = 
  withFile (optFileName opts) ReadMode (rand (optCount opts) (optUrl opts) (optOutFileName opts))


  -- httpSink request $ \response -> 
  --   CL.mapM_ (\s -> S.hPut stdout "\n----START---\n" >> S.hPut stdout s >> S.hPut stdout "\n----END---\n")

rand :: Int -> String -> String -> Handle -> IO ()
rand count url out h = do
  t <- hGetContents h
  request' <- parseRequest url
  let reqBody = object [ "text" .= t, "count" .= count]
      request = setRequestMethod "POST" $ setRequestBodyJSON reqBody $ request'
  response <- httpJSON request :: IO (Response [String])

  ct <- getPOSIXTime
  let sheet = foldl (\a b -> a & b) def $ map (\(i, txt) -> cellValueAt (i,1) ?~ CellText txt) $ zip [1..] (map T.pack $ getResponseBody response)
      xlsx = def & atSheet "List1" ?~ sheet

  L.writeFile out $ fromXlsx ct xlsx
  