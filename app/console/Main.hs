module Main where

import Generator

import Control.Monad.Reader
import Options.Applicative

optParser :: Parser Config
optParser = Config
        <$> argument str (help "URL of data")


main :: IO ()
main = do
  ops <- execParser $ info (optParser <**> helper) (fullDesc <> progDesc "xml generator for avito" <> header "xmlgen - xml generator for avito") 
  res <- runReaderT generateXML ops
  case res of
    Left err -> putStrLn err
    Right xml -> putStr xml
