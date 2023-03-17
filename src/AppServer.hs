module AppServer where 

import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Network.WebSockets
import           Protocol
import           Servant                  hiding (route)
import           Servant.API.WebSocket

type API = "onyx" :> WebSocketPending     
    :<|> Raw

myAPI :: Proxy API
myAPI = Proxy

onyx :: MonadIO m => PendingConnection -> m ()
onyx pending = do
  conn <- liftIO $ acceptRequest pending
  liftIO $ print ("connected" :: Text)
  liftIO $ withPingThread conn 30 (return ()) $ do
    forever $ do
      msg <- receiveData conn :: IO BS.ByteString 
      let test = eitherDecode msg :: Either String Instruction
      liftIO $ print test
      sendTextData conn msg

app' :: Server API
app' = onyx :<|> serveDirectoryFileServer "site"
  

app :: Application
app = serve myAPI app'