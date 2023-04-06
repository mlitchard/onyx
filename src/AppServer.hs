module AppServer where 

import Prelude hiding (StateT, ReaderT,lift,gets)
import           Account
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.Default
import           Data.Map (insert,lookup)
import           Data.Proxy
import           Data.Text hiding (filter)
import           Data.Time.Clock ( UTCTime, getCurrentTime )
import           GHC.Generics
import           LoginForm
import           Network.Wai (Middleware)
import           Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.RequestLogger.JSON
import qualified Network.WebSockets as WS
import           Protocol
import           Servant                  hiding (route)
import           Servant.API.WebSocket 
import           Servant.Auth.Server as SAS
import           System.Log.FastLogger


type EnvM = StateT MetaState Handler  
type AppM = ReaderT AppCtx EnvM 

type LoggedInMap = Map Text Text 

data MetaState  = MkMetaState
  { _loggedIn :: LoggedInMap
  , _openRooms :: Int
  }

data AppCtx = MkAppCtx {
  _getConfig :: SiteConfig
  , _dummyAccounts :: ![User]
  , _getLogger :: LoggerSet
} 

data SiteConfig = MkSiteConfig
  { environment :: !Text
  , version :: !Text 
  , adminAccount :: Admin
  } deriving stock (Generic,Show)

data LogMessage = LogMessage {
  message        :: !Text
  , timestamp    :: !UTCTime
  , level        :: !Text
  , lversion     :: !Text
  , lenvironment :: !Text
} deriving stock (Eq, Show, Generic)

instance FromJSON LogMessage
instance ToJSON LogMessage where
  toEncoding = genericToEncoding defaultOptions

instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode

type API = "onyx" :> WebSocketPending     
    :<|> Raw

loginHandler :: CookieSettings
             -> JWTSettings
             -> LoginForm
             -> AppM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LogMessage)
loginHandler cookieSettings jwtSettings form@(MkLoginForm {..}) = do
  config     <- asks _getConfig
  accounts   <- asks _dummyAccounts
  logset     <- asks _getLogger
  tstamp <- liftIO getCurrentTime
  let logMsg = LogMessage { message = "AdminUser login attempt failed!"
                          , timestamp = tstamp
                          , level = "info"
                          , lversion = version config
                          , lenvironment = environment config
                          }

  case validated (config,accounts) form of
    Nothing -> do
--       liftIO $ pushLogStrLn logset $ toLogStr logMsg
      throwError err401
    Just usr -> do
      mApplyCookies <- lift $ liftIO $ SAS.acceptLogin cookieSettings jwtSettings usr
      case mApplyCookies of
        Nothing           -> do
  --        liftIO $ pushLogStrLn logset $ toLogStr logMsg
          throwError err401
        Just applyCookies -> do
{-
          loggedInMap <- get _loggedIn
          logStatusMsg <- case (updateMap nameField loggedInMap) of
	                    Left msg -> pure msg
			    Right uMap -> do
			                   put uMap
					   pure (nameField <> "authenticated")
-}                          
          liftIO $ pushLogStrLn logset 
	    $ toLogStr (logMsg{message = logStatusMsg})
          pure $ applyCookies $ toLogStr (logMsg{message = logStatusMsg})
	  where logStatusMsg = "placeholder"

updateMap :: Text -> LoggedInMap -> Either Text LoggedInMap
updateMap name lmap = case (lookup name lmap) of
  Just name -> Left $ (name <> "is already authenticated")
  Nothing   -> Right $ insert name (name <> " successfully authenticated") lmap

validateAdmin :: SiteConfig -> LoginForm -> Maybe Authenticated
validateAdmin (MkSiteConfig {..}) (MkLoginForm {..}) = 
  if (nameField == adminUserName) && (passwordField == adminPassWord)
    then Just $ AuthenticatedAdmin adminAccount
    else Nothing
  where
    adminUserName = (name . admin) adminAccount
    adminPassWord = (password . admin) adminAccount

validateUser :: [User] -> LoginForm -> Maybe Authenticated
validateUser users (MkLoginForm {..}) = case lookupUser of
  [u] -> if ((password . user) u == passwordField) 
           then (Just $ AuthenticatedUser u) 
           else Nothing
  _   -> Nothing
  where
    lookupUser = filter (\u -> ((name . user) u) == nameField) users

validated :: (SiteConfig,[User]) -> LoginForm -> Maybe Authenticated 
validated (config,accounts) lf = 
  validateUser' lf <|> validateAdmin' lf
  where
    validateUser'  = validateUser accounts
    validateAdmin' = validateAdmin config

myAPI :: Proxy API
myAPI = Proxy

-- authenticatedAdmin :: SiteConfig -> LoginForm -> Maybe Authenticated
-- authenticatedAdmin 

onyx :: MonadIO m => WS.PendingConnection -> m ()
onyx pending = do
  conn <- liftIO $ WS.acceptRequest pending
  liftIO $ print ("connected" :: Text)
  liftIO $ WS.withPingThread conn 30 (return ()) $ do
    forever $ do
      msg <- WS.receiveData conn :: IO BS.ByteString 
      let test = eitherDecode msg :: Either String Instruction
      liftIO $ print test
      WS.sendTextData conn msg


app' :: Server API
app' = onyx :<|> serveDirectoryFileServer "site"
  

app :: Application
app = serve myAPI app'
