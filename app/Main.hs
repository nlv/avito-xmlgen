module Main where

import Lib

import Options.Applicative

main :: IO ()
main = 
  let ops = argument str (help "URL of data") in do
  src <- execParser $ info (ops <**> helper) (fullDesc <> progDesc "Converting Avito posts" <> Options.Applicative.header "header")
  res <- generateXML src
  case res of
    Left err -> putStrLn err
    Right xml -> putStr xml
