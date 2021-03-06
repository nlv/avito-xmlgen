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


import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad.IO.Class

type Api = Capture "src" T.Text :> Servant.Get '[PlainText, JSON] (Headers '[Header "Content-Disposition" String] T.Text)

api :: Proxy Api
api = Proxy

main :: IO ()
main = do
  let port = 3031
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
server = \src -> fmap (addHeader "attachment; filename=Ads.xml") (getFile src)

getFile :: T.Text -> Handler T.Text
getFile src = do
  res <- liftIO $ generateXML src
  case res of
    Left err -> pure $ T.pack err
    Right xml -> pure $ T.pack xml
