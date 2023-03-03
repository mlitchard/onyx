module Protocol where 

import Protocol.Protocol

data Instruction 
  = MkInstruction {
      instructionAID :: Text 
    , instruction :: Either Meta Command
    } deriving (Generic,Ord,Eq, Show)