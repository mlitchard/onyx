module Account where

import Data.Aeson
import Servant.Auth.Server as SAS

newtype Admin = MkAdmin {admin :: Account} deriving stock (Show,Eq,Ord,Generic)
newtype User = MkUser {user :: Account} deriving stock (Show,Eq,Ord,Generic)

data Authenticated 
  = AuthenticatedAdmin Admin
  | AuthenticatedUser User
    deriving stock (Show,Eq,Ord,Generic)

instance SAS.ToJWT Authenticated
instance SAS.FromJWT Authenticated

instance ToJSON Authenticated where
  toJSON auth = 
    object [("authenticated")
      .= object[(authLevel .= authAccount)] ]
    where
      (authLevel, authAccount) = 
        case auth of
          (AuthenticatedAdmin account') -> ("admin", admin account')
          (AuthenticatedUser account')  -> ("user", user account')

instance FromJSON Authenticated where
  parseJSON = withObject "Authenticated" $ \o -> do
    res <- o .: "authenticated"
    let admAcct = res .: "admin"
        usrAcct = res .: "user"
    AuthenticatedUser <$> MkUser <$> usrAcct
      <|> AuthenticatedAdmin <$> MkAdmin <$> admAcct

data Account
  = MkAccount {
      name :: Text
    , password :: Text
    } deriving stock (Show,Eq,Ord,Generic)

instance ToJSON Account where
  toJSON (MkAccount {..}) = 
      object [("name" .= toJSON name), ("password" .= toJSON password)]

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
