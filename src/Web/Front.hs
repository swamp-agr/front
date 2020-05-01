
module Web.Front where

import           Data.Text                 (Text)

import           Bridge
import           Text.Blaze.Front
import           Text.Blaze.Front.Renderer

import qualified Data.Text                 as T

data ClientTaskType = OnlyHtml | OnlyEvents | BothHtmlEvents | None
  deriving (Eq, Show)

-- | Generate message that will be pushed to client(s) based on underlying communication.
createTask
  :: Show a
  => Text -- ^ DOM Element Id.
  -> ClientTaskType -- ^ Type of task to execute on client.
  -> (t -> Markup a) -- ^ How to render state.
  -> t -- ^ State to render.
  -> ClientTask a -- ^ Message that will be pushed to client(s).
createTask eid taskType renderer state = task
  where rhtml = AttachText eid (T.pack . renderHtml $ markup)
        markup = renderer state
        (html, events) = case taskType of
          OnlyHtml       -> ([rhtml], mempty)
          OnlyEvents     -> ([], markup)
          BothHtmlEvents -> ([rhtml], markup)
          None           -> ([], mempty)
        task = ClientTask
          { executeRenderHtml = html
          , executeAction = registerEvents events []
          , executeScript = []
          }

emptyTask :: ClientTask a
emptyTask = ClientTask { executeRenderHtml = [], executeAction = [], executeScript = [] }
