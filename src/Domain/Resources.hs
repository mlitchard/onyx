{-# LANGUAGE InstanceSigs #-}
module Domain.Resources where

import Data.Aeson
import Data.Char
import Text.Show (Show (..))

data ResourceName = SpecialK | InterzoneSpecial | BabyBlue 
  deriving stock (Generic,Ord,Eq,Enum,Bounded)

instance Show ResourceName where 
  show SpecialK = "specialk"
  show InterzoneSpecial = "interzonespecial"
  show BabyBlue = "babyblue"

instance ToJSON ResourceName where
  toJSON = genericToJSON defaultOptions { 
    constructorTagModifier = map toLower
  }

instance FromJSON ResourceName where
  parseJSON = genericParseJSON defaultOptions { 
    constructorTagModifier = map toLower
  }

instance ToText ResourceName where 
  toText :: ResourceName -> Text
  toText SpecialK         = "specialk"
  toText InterzoneSpecial = "interzonespecial"
  toText BabyBlue         = "babyblue"

data Transaction = Buy | Sell
  deriving stock (Generic,Ord,Eq,Enum,Bounded)

instance Show Transaction where 
  show Buy = "buy"
  show Sell = "sell"

instance ToJSON Transaction where
  toJSON = genericToJSON defaultOptions { 
    constructorTagModifier = map toLower
  }

instance FromJSON Transaction where
  parseJSON = genericParseJSON defaultOptions { 
    constructorTagModifier = map toLower
  }

instance ToText Transaction where 
  toText :: Transaction -> Text
  toText Buy = "buy"
  toText Sell = "sell"
  
data ResourceTransaction = MkResourceTransaction 
  { transaction   :: Transaction
  , resourceName  :: ResourceName
  , amount        :: Int
  } deriving stock (Generic,Ord,Eq,Show)

instance ToJSON ResourceTransaction where 
  toJSON (MkResourceTransaction {..}) =
    object [("resourcetransaction" :: Key) .= 
      object [ ("transaction" :: Key) .= transaction
             , ("resource" :: Key)    .= resourceName
             , ("amount" :: Key)      .= amount
             ]
    ]
    
instance FromJSON ResourceTransaction where 
  parseJSON = withObject "Resource Transaction" $ \o -> do 
    res :: Object <- o .: "resourcetransaction"
    transaction   <- res .: "transaction" 
    resourceName  <- res .: "resource"
    amount        <- res .: "amount" 
    pure $ MkResourceTransaction {..}