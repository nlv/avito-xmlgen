{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative

import System.IO

import           Data.Aeson
import           Network.HTTP.Simple

data Opts = Opts {
    optFileName :: String
  , optCount   :: Int
}

optParser :: Parser Opts
optParser = Opts 
              <$> argument str (help "Filename of data")
              <*> option auto ( long "count" <> short 'c' <> help "Count of randomized cases")

main :: IO ()
main = run =<< execParser opts
    where opts = info (optParser <**> helper)
                    ( fullDesc
                    <> progDesc "Randomize text"
                    <> header "Randomize text")

run :: Opts -> IO ()
run opts = 
  withFile (optFileName opts) ReadMode (rand $ optCount opts)

rand :: Int -> Handle -> IO ()
rand count h = do
  t <- hGetContents h
  let reqBody = object [ "text" .= t, "count" .= count]
      request = setRequestBodyJSON reqBody $ "POST http://randtext.localhost"
  response <- httpJSON request :: IO (Response Value)
  putStrLn (show response)
  