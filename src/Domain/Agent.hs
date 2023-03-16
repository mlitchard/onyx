module Domain.Agent where

import Domain.Planet       (PlanetName)
import Data.UUID    (UUID)

data Agent =
    Agent
        { _userId :: UUID
        , _userName :: Text 
        } 

newtype AgentMap = AgentMap (Map Int Text) deriving stock (Show)

newtype LocationMap = LocationMap (Map Int PlanetName) deriving stock (Show)