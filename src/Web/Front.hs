
module Web.Front where

import           Data.Text                 (Text)

import           Bridge
import           Text.Blaze.Front
import           Text.Blaze.Front.Renderer

import qualified Data.Text                 as T

-- | Generate message that will be pushed to client(s) based on underlying communication.
createTask
  :: Show a
  => Text -- ^ DOM Element Id.
  -> (t -> Markup a) -- ^ How to render state.
  -> t -- ^ State to render.
  -> ClientTask a -- ^ Message that will be pushed to client(s).
createTask eid renderer state = task
  where rhtml = AttachText eid (T.pack . renderHtml $ markup)
        markup = renderer state
        task = ClientTask
          { executeRenderHtml = [rhtml]
          , executeAction = registerEvents markup []
          }

emptyTask :: ClientTask a
emptyTask = ClientTask { executeRenderHtml = [], executeAction = [] }
