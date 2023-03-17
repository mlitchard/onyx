module Agent where

import Data.UUID 
import Domain.Agent

newtype AID = AID UUID deriving stock Show

newtype AgentMap = AgentMap (Map AID Agent) deriving stock (Show)