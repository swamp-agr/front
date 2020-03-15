{-# LANGUAGE RankNTypes #-}
module Web.Front.Broadcast where

import           Bridge
import           Conduit
import           Control.Concurrent.Async
import           Control.Concurrent.STM   as STM
import           Control.Monad            (forever)
import           Data.Aeson               (Value, decode, toJSON)
import           Data.Aeson.Text
import qualified Data.ByteString.Lazy     as BL
import           Data.Data
import           Data.Text.Encoding       (encodeUtf8)
import           Data.Text.Lazy.Builder   (toLazyText)
import           Fay.Convert              (showToFay)
import           Network.WebSockets       hiding (Headers)

-- | The common way how to use websocket 'Connection' obtained from 'Handler' via 'Conduit'.
-- 'interact' starts two concurrent processes.
-- First one is responsible for reading data from stream, decoding JSON message, executing custom business logic implemented by user and pushing the produced outgoing 'message' to 'TChan'.
-- The second process is constantly reading from 'TChan', encoding the given message and pushing it to all subscribers.
interact
  :: (Data message, Data message2)
  => (Value -> cache model -> ClientId -> IO (Out (Action message)))
  -> Connection
  -> TChan (Out (Action message))
  -> TChan (Out message2)
  -> cache model
  -> ClientId
  -> IO ()
interact onCommand stream in' out' tvar client = do
  race_
    (readLoop stream in' tvar client)
    (writeLoop stream out' client)

  where
    writeLoop
      :: Data message => Connection -> TChan (Out message) -> ClientId -> IO ()
    writeLoop _stream _out _client = forever $ liftIO $ do
      cmd <- atomically $ readTChan _out
      json <- pure $ toJSON $ showToFay cmd
      case cmd of
        EmptyCmd ->
          sendTextData _stream (toLazyText $ encodeToTextBuilder json)
        ExecuteClient cid task strategy -> do
          let sid = _client
          if sid == cid && strategy == ExecuteExcept
            then do
              json2 <- pure $ toJSON $ showToFay $
                ExecuteClient cid task ExecuteExcept
              sendTextData _stream (toLazyText $ encodeToTextBuilder json2)
            else do
              json2 <- pure $ toJSON $ showToFay $
                ExecuteClient cid task ExecuteAll
              sendTextData _stream (toLazyText $ encodeToTextBuilder json2)

    readLoop _stream _in _tvar _client = forever $ liftIO $ do
      data' <- receiveData _stream
      runConduit $ yield data' .| mapM_C (\cmdstr -> do
        case (decode $ BL.fromChunks [encodeUtf8 cmdstr] :: Maybe Value) of
          Nothing -> error "No JSON provided"
          Just cmd -> do
            liftIO $ putStrLn $ show cmd
            res <- onCommand cmd _tvar _client
            atomically $ writeTChan _in res)
