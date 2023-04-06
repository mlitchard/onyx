module LoginForm where

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

instance FromJSON Account where
  parseJSON = withObject "Account" $ \o -> do
    name <- o .: "name"
    password <- o .: "password"
    pure $ MkAccount {..}

instance ToJSON Admin where
  toJSON (MkAdmin {..}) = 
    object ["admin" .= toJSON admin]

instance FromJSON Admin where
  parseJSON = withObject "Admin" $ \o -> do
    res <- o .: "admin"
    admin <- parseJSON res
    pure $ MkAdmin {..}

instance ToJSON User where
  toJSON (MkUser {..}) = 
    object ["user" .= toJSON user]

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> do
    res <- o .: "user"
    user <- parseJSON res
    pure $ MkUser {..} 

