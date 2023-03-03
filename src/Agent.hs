module Agent where 

import Planet (PlanetName)

newtype AgentMap = AgentMap (Map Int Text) deriving stock (Show)
newtype LocationMap = LocationMap (Map Int PlanetName) deriving stock (Show)