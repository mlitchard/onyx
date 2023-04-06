module LoginForm where

import Account
import Data.Aeson
import Data.Text

data LoginForm
  = MkLoginForm {
      nameField :: Text
    , passwordField :: Text
    } deriving stock (Show,Eq,Generic)

instance ToJSON LoginForm where
  toJSON (MkLoginForm {..}) = 
      object ["login" 
        .= jNameField, jPasswordField]
    where
      jNameField :: (Key,Value)
      jNameField = "name" .= toJSON nameField

      jPasswordField :: (Key,Value)
      jPasswordField = "password" .= toJSON passwordField

instance FromJSON LoginForm where
  parseJSON = withObject "LoginForm" $ \o -> do
    res <- o .: "login"
    nameField <- res .: "name"
    passwordField <- res .: "password"
    pure $ MkLoginForm {..}
