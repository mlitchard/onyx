module Domain.Game where

import Control.Concurrent.STM.TChan (TChan)
import Data.UUID 
import Domain.Planet ( LocationMap, PlanetMap )
import Reactive.Banana.Frameworks ( AddHandler, MomentIO, fromAddHandler, reactimate )
import Agent (AgentMap)
import Protocol.Protocol ( UAC, InitMaps, VAC )
import Reactive.Banana

newtype GameID = GameID UUID deriving stock Show 
data GameState 
  = GameState 
      { _gameId       :: GameID
      , _agentMap     :: !AgentMap
      , _planetMap    :: !PlanetMap
      , _locationMap  :: !LocationMap
      , _gameIsOver   :: Bool
      } deriving stock (Show)

data Parameters = Parameters
  { _input        :: AddHandler [UAC] -- All user input per tick
  , _output       :: TChan GameState  -- 
  , _initMaps     :: InitMaps         -- provides initial states
  , _tick         :: AddHandler ()    -- provides heartbeat
  }

makeNetworkDescription :: Parameters -> MomentIO ()
makeNetworkDescription params = mdo
    _eInput <- fromAddHandler (_input params)
    eTick <- fromAddHandler (_tick params)


    -- Buffer things
    let eBuffer :: Event [VAC]
        eBuffer = bBuffer <@ eTick

        eClearBuffer :: Event [VAC]
        eClearBuffer = [] <$ eBuffer

        eValidated :: Event [VAC]
        eValidated = error "eValidated undefined"

    bBuffer <- accumB [] $ manageBuffer <$> unionWith validateFirst eValidated eClearBuffer

    reactimate $ print <$> eBuffer

validateFirst :: [VAC] -> [VAC] -> [VAC]
validateFirst validated _ = validated

manageBuffer :: [VAC] -> [VAC] -> [VAC]
manageBuffer [] _ = []
manageBuffer incoming acc = acc ++ incoming
