{-# LANGUAGE InstanceSigs #-}
module Domain.Planet where

import Data.Char ( toLower )
import Data.Aeson
    ( genericParseJSON,
      defaultOptions,
      genericToJSON,
      FromJSON(parseJSON),
      Options(constructorTagModifier),
      ToJSON(toJSON), Value )
import Data.Aeson.Types (Parser)

data PlanetName
  = Vulcan
  | Mongo
  | Arakis
  | Dantooine
  | Tatooine
  | VoidlessVoid
  deriving stock (Generic,Ord,Eq,Enum,Bounded,Show)

instance ToJSON PlanetName where
  toJSON :: PlanetName -> Value
  toJSON = genericToJSON defaultOptions { 
    constructorTagModifier = map toLower
  }
      
instance FromJSON PlanetName where
  parseJSON :: Value -> Parser PlanetName
  parseJSON = genericParseJSON defaultOptions { 
    constructorTagModifier = map toLower
  }

data Planet 
  = Planet 
    { residents :: ![AID]
    , description :: ToText
    , }

newtype PlanetMap = PlanetMap (Map PlanetName Text) deriving stock (Show)