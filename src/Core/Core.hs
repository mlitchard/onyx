module Core.Core where


import Control.Concurrent.STM.TChan (TChan)
import Planet ( PlanetMap )
import Reactive.Banana.Frameworks ( AddHandler, MomentIO, fromAddHandler, reactimate )
import Agent (AgentMap)
import Protocol.Protocol ( UAC, InitMaps, VAC )
import Reactive.Banana

data GameState = GameState !AgentMap !PlanetMap deriving stock (Show)
data Parameters = Parameters
  { input        :: AddHandler [UAC] -- All user input per tick
  , output       :: TChan GameState  -- 
  , initMaps     :: InitMaps         -- provides initial states
  , tick         :: AddHandler ()    -- provides heartbeat
  }

makeNetworkDescription :: Parameters -> MomentIO ()
makeNetworkDescription params = mdo
    _eInput <- fromAddHandler (input params)
    eTick <- fromAddHandler (tick params)


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