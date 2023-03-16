module Server.Server where 

import           Data.Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as BS
import           Data.UUID


newtype Base64 = Base64 {unBase64 :: BS.ByteString}

makeBase64 :: Text -> Base64  
makeBase64 = Base64 . B64.encode . encodeUtf8

instance ToJSON Base64 where 
    toJSON = String . decodeUtf8 . unBase64
{-
instance FromJSON Base64 where 
    parseJSON = withText "Base64" $ \x -> pure $ Base64 x
-}
newtype ClientName = MkClientName {unClientName :: Text} deriving stock Show

makeClientName :: Text -> ClientName  
makeClientName = MkClientName 
{-
instance ToText ClientName where 
    toText = decodeUtf8 . B64.decodeLenient . unBase64 . unClientName 
-}
 
data MessageTextIn = MkMessageTextIn {
    aid             :: UUID
  , messageTextIn :: Text 
  }

makeMessageTextIn :: UUID -> Text -> MessageTextIn
makeMessageTextIn aid msg = MkMessageTextIn {
    aid = aid
  , messageTextIn = msg
}

instance ToJSON MessageTextIn where 
    toJSON (MkMessageTextIn aid messageBS) = 
        object ["player_message" .= (object ["message" .= messageBS, "aid" .= aid]) ]

instance FromJSON MessageTextIn where
    parseJSON = withObject "MessageTextIn" $ \o -> do
        messageTextIn <- o .: "player_message"
        aid <- o .: "aid"
        pure $ MkMessageTextIn {..}

data MessageTextOut = MkMesageTextOut {aidMTO :: UUID, unMessageTextOut :: Text} 

{-
data Message = Notice MessageTextIn
             | Tell ClientName MessageTextIn 
             | Broadcast ClientName 
             | Command BS.ByteString
             deriving stock Show
-}