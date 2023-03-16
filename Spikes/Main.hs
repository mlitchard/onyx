module Main where

import Control.Concurrent (threadDelay)
import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Network.WebSockets
import Network.Wai.Handler.Warp
import Servant.API
import           Servant.API.WebSocket
import Servant.HTML.Blaze
import Servant.Server
import Text.Blaze.Html
import Text.Hamlet

data Test = Test Int String
  deriving stock (Generic)

instance ToJSON Test

type TestAPI 
    = "gameinit" :> WebSocketPending
    :<|> "tests" :> Get '[JSON] [Test]
    :<|> "Test.html" :> Get '[HTML] Html
  

main :: IO ()
main = run 8080 (serve (Proxy :: Proxy TestAPI) serverTestAPI)

serverTestAPI :: Server TestAPI
serverTestAPI = gameinit :<|> tests :<|> testHtml

-- server :: Server WebSocketApi
-- gameinit :: MonadIO m => PendingConnection -> m ()
gameinit :: PendingConnection -> Handler ()
gameinit pc = do
  c <- liftIO $ acceptRequest pc
  liftIO $ forkPingThread c 10
  liftIO . forM_ [1..] $ \i ->
    sendTextData c (pack $ show (i :: Int)) >> threadDelay 1000000
  return ()
    

tests :: Handler [Test]
tests = return [ Test 1 "Test 1"
               , Test 2 "Test 2"
               ]
testHtml :: Handler Html
testHtml = return [shamlet|
  $doctype 5
    <html>
     <head>
       <title>This is a title
     <body>
       <p>This is text
  |]
