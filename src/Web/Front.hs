
module Web.Front where

import Control.Monad.Reader
import Data.Text (Text)
import Network.WebSockets

import Bridge
import Text.Blaze.Front
import Text.Blaze.Front.Renderer

import qualified Data.Text as T

type WebSocketsT = ReaderT Connection
  
data Front m cache state action channel value handler any = Front
  { initState :: handler (cache state)
  , action :: action
  , interact :: channel Out -> channel Out -> cache state -> WebSocketsT handler ()
  , onReceive :: value -> cache state -> WebSocketsT (m any) (Out action)
  , render :: state -> Markup action
  } 

createTask
  :: Show a
  => Text
  -> (t -> Markup a)
  -> t
  -> ClientTask a
createTask eid renderer state = task
  where rhtml = AttachText eid (T.pack . renderHtml $ markup)
        markup = renderer state
        task = ClientTask
          { executeRenderHtml = [rhtml]
          , executeAction = registerEvents markup []
          }
