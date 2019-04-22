{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module YesodTodo where

import           Conduit
import           Control.Concurrent.STM.Lifted as STM
import           Control.Monad.Reader
import           Prelude                       hiding (interact)
import           System.Random                 (randomRIO)
import           Text.Blaze.Front.Renderer     (renderNewMarkup)
import           Yesod.Core
import           Yesod.Static
import           Yesod.WebSockets

import qualified Data.Text                     as T

import           Bridge
import           Todo
import           Web.Front.Broadcast

-- * App

data App = App
  { appModel   :: TVar Model
  , appChannel :: TChan (Out (Action Msg))
  , appStatic  :: Static
  }

mkYesod "App" [parseRoutes|
/ HomeR GET
/static      StaticR  Static  appStatic
|]

-- * API

instance Yesod App where
  defaultLayout widget = do
    pc <- widgetToPageContent [whamlet|^{widget}|]
    withUrlRenderer [hamlet|<html>
  <head>
    ^{pageHead pc}
    <script src="/static/bundle.js">
  <body>
    <div #root>^{pageBody pc}
|]

-- * Handler for /

getHomeR :: Handler Html
getHomeR = do
  modelTVar <- appModel <$> getYesod
  model <- STM.readTVarIO modelTVar
  webSockets $ webApp modelTVar
  defaultLayout $ do
    setTitle "TODO"
    toWidget $ renderNewMarkup $ renderModel model

-- * WebSockets Web application

webApp :: TVar Model -> WebSocketsT (HandlerFor App) ()
webApp tvar = do
  conn <- ask
  cid <- clientSession
  writeChan' <- appChannel <$> getYesod
  readChan' <- atomically $ do
    dupTChan writeChan'
  liftIO $ interact conn writeChan' readChan' tvar cid

-- * Helpers

clientSession :: MonadHandler m => m Int
clientSession = do
  res <- lookupSession "client"
  case res of
    Nothing -> do
      (rnd :: Int) <- liftIO $ randomRIO (0,200000)
      let clientid = T.pack . show $ rnd
      setSession "client" clientid
      return rnd
    Just cid -> return $ parseInt cid

-- * Main

main :: IO ()
main = do
  (App
    <$> STM.newTVarIO initModel
    <*> atomically newBroadcastTChan
    <*> staticDevel "./static")
  >>= warp 3000
