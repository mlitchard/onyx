module Server.Server where 

import           Conduit
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Conduit
import           Data.Conduit.Network
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as BS
import           Data.Conduit.TMChan
import qualified Data.Map               as Map
import qualified Data.Text        as Text
import           Data.Text.Encoding (decodeLatin1)
import           Data.Word8               (_cr)
import           Control.Concurrent.Async (concurrently)
import           Control.Monad
import           Control.Exception        (finally)
import           Relude 
import           Relude.String.Conversion
import           Text.Printf              (printf)
import           Text.Show

newtype Base64 = Base64 {unBase64 :: BS.ByteString}

makeBase64 :: Text -> Base64  
makeBase64 = Base64 . B64.encode . encodeUtf8

instance ToJSON Base64 where 
    toJSON = String . decodeUtf8 . unBase64
{-
instance FromJSON Base64 where 
    parseJSON = withText "Base64" $ \x -> pure $ Base64 x
-}
newtype ClientName = MkClientName {unClientName :: Base64}

makeClientName :: Text -> ClientName  
makeClientName = MkClientName . makeBase64 

instance ToText ClientName where 
    toText = decodeUtf8 . B64.decodeLenient . unBase64 . unClientName 

instance Show ClientName where 
    show = (decodeUtf8 @String @ByteString) 
            . B64.decodeLenient 
            . unBase64 
            . unClientName 
     
data MessageTextIn = MkMessageTextIn {
    aid             :: Text
  , unMessageTextIn :: Base64
  }

makeMessageTextIn :: Text -> Text -> MessageTextIn
makeMessageTextIn aid msg = MkMessageTextIn {
    aid = aid
  , unMessageTextIn =  makeBase64 msg
}

instance Show MessageTextIn where 
    show (MkMessageTextIn {..}) = "MkMessageTextIn{ aid = " <> (toString aid) <> ", "
     <> "unMessageTextIn = " <> "\"" <> f <> "\"" <> " }"
     where f = ((decodeUtf8 @String @ByteString) 
                . B64.decodeLenient . unBase64) unMessageTextIn

instance ToJSON MessageTextIn where 
    toJSON (MkMessageTextIn aid messageBS) = 
        object ["player_message" .= (object ["message" .= messageBS, "aid" .= aid]) ]
        where
            messageBS' = (B64.encode . unBase64) messageBS 

instance FromJSON MessageTextIn where
    parseJSON = withObject "MessageTextIn" $ \o -> do
        player_message' <- o .: "player_message"
        aid <- player_message' .: "aid"
        unMessageTextIn :: Base64 <- f <$> player_message' .: "message"
        pure $ MkMessageTextIn {..}
        where
            f :: Value -> Base64
            f (String bs) = Base64 (encodeUtf8 bs) 
            f _ = error "wut"


newtype MessageTextOut = MkMesageTextOut {unMessageTextOut :: BS.ByteString}



data Message = Notice MessageTextIn
             | Tell ClientName MessageTextIn 
             | Broadcast ClientName 
             | Command BS.ByteString
             deriving stock Show