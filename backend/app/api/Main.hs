{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Generator

import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import Network.HTTP.Types.Method

import System.IO
import Control.Monad.Reader


import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad.IO.Class
import Options.Applicative

data AppConfig = AppConfig {
    appConfPort :: Int
}

optParser :: Parser AppConfig
optParser = AppConfig
        <$> option auto ( long "port" <> short 'p' <> help "port")

type Api = QueryParam "src" T.Text :> QueryParam "skip" Int :> QueryFlag "shortUrl" :> Servant.Get '[PlainText, JSON] (Headers '[Header "Content-Disposition" String] T.Text)

api :: Proxy Api
api = Proxy

main :: IO ()
main = do
  ops <- execParser $ info (optParser <**> helper) (fullDesc <> progDesc "API for avito xml generator" <> header "xmlgen-api - API for avito xml generator") 
  let port = appConfPort ops
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ cors (const $ Just policy) $ provideOptions api $ serve api server
  where
    policy = simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ], corsMethods = [methodGet, methodPost, methodDelete, methodOptions] }

server :: Server Api
server = \src skip shortUrl -> fmap (addHeader "attachment; filename=Ads.xml") (getFile $ Config {confSrc = maybe "" id src, confSkip = maybe 0 id skip, shortUrl = shortUrl})

getFile :: Config -> Handler T.Text
getFile config = do
  -- liftIO $ putStrLn $ "Запустили"
  res <- liftIO $ runReaderT generateXML $ config
  case res of
    Left err -> pure $ T.pack err
    Right xml -> pure $ T.pack xml
