module Protocol.Protocol where

import Domain.Agent (AgentMap, LocationMap)
import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Domain.Planet (PlanetName, PlanetMap)
import Domain.Resources (ResourceTransaction)

data ChatEmote 
  = Roar 
  | Grin 
  | Smirk 
  | Wave 
  | Complain 
  | Cackle 
      deriving (Generic,Ord,Eq,Enum,Bounded,Show)

instance ToText ChatEmote where 
  toText Roar     = "Roar"
  toText Grin     = "Grin"
  toText Smirk    = "Smirk"
  toText Wave     = "Wave"
  toText Complain = "Complain"
  toText Cackle   = "Cackle"

instance ToJSON ChatEmote where
  toJSON = 
    genericToJSON defaultOptions {
      constructorTagModifier = map toLower
    }

instance FromJSON ChatEmote where
  parseJSON = genericParseJSON defaultOptions { 
    constructorTagModifier = map toLower
  }

data Command
  = Move PlanetName
  | Zap Text 
  | Look 
  | Transaction ResourceTransaction
    deriving stock (Generic,Ord,Eq,Show)

instance ToJSON Command where
  toJSON (Move planetname) = object ["command" .= (("move" .= planetname) :: Object)]
  toJSON (Zap aid)         = object ["command" .= (("zap" :: Key) .= aid :: Object)]
  toJSON Look              = object ["command" .= ("look" :: Text)] 
  toJSON (Transaction rt)  = object ["command" .= rt]

instance FromJSON Command where
  parseJSON = withObject "command" $ \o -> do
    command <- o .: "command"
    parseMove command -- <|> parseResourceTransaction command 
      <|> parseZap command 
      <|> parseLook command 
      <|> parseResourceTransaction command
     -- <|> parseMove command

parseMove :: Value -> Parser Command 
parseMove (Object obj) = do 
  move <- obj .: "move"
  Move <$> parseJSON move   
parseMove val = fail ("not a move " <> (show val))

parseZap :: Value -> Parser Command 
parseZap (Object obj) = do 
  aid <- obj .: "zap"
  pure (Zap aid)
parseZap _ = fail "malformed zap"

parseLook :: Value -> Parser Command 
parseLook (String "look")= pure Look
parseLook _ = fail "malformed look" 

parseResourceTransaction :: Value -> Parser Command 
parseResourceTransaction rt@(Object obj) = Transaction <$> parseJSON rt
parseResourceTransaction _ = fail "not a ResourceTransaction"
-- Validated Player command
newtype VAC = VAC Command deriving stock (Eq,Ord,Show)
-- Unvalidated Player command
newtype UAC = UAC Command deriving stock (Eq,Ord,Show)

data InitMaps  = InitMaps 
  { aMap :: AgentMap
  , pMap :: PlanetMap
  , lMap :: LocationMap
  }

data Meta = Say Text | Emote ChatEmote | Quit deriving (Generic,Ord,Eq,Show)

instance ToJSON Meta where
  toJSON (Say said)    = object ["meta" .= (("say" .= said) :: Object)]
  toJSON (Emote emote) = object ["meta" .= (("emote" :: Key) .=  (toText emote) :: Object)]
  toJSON Quit          = object ["meta" .= ("quit" :: Text)]

instance FromJSON Meta where
  parseJSON = withObject "meta" $ \o -> do
    command <- o .: "meta"
    parseSay command 
      <|> parseEmote command 
      <|> parseQuit command

parseSay :: Value -> Parser Meta 
parseSay (Object val) = do 
   said <- val .: "say" 
   pure (Say said) 
parseSay _ = fail "malformed say"

parseEmote :: Value -> Parser Meta 
parseEmote (Object val) = do 
  emote <- val .: "emote"
  emote' <- parseJSON emote :: Parser ChatEmote
  pure (Emote emote')
parseEmote _ = fail "malformed emote"

parseQuit :: Value -> Parser Meta 
parseQuit (String "quit")= pure Quit
parseQuit _ = fail "malformed quit"