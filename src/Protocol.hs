module Protocol where 

import Data.Aeson 
import Protocol.Protocol

data Instruction 
  = MkInstruction {
      instructionAID :: Text 
    , instruction :: Either Meta Command
    } deriving (Generic,Ord,Eq, Show)

instance ToJSON Instruction where 
  toJSON (MkInstruction {..}) = 
    object [("instruction-data")
    .= object [("aid" .= toJSON instructionAID), "instruction" 
         .= (either (toJSON :: Meta -> Value) 
                    (toJSON :: Command -> Value) 
                    instruction)]]

instance FromJSON Instruction where
    parseJSON = withObject "Instruction" $ \o -> do
        res <- o .: "instruction-data"
        aid <- res .: "aid"
        instruct <- res .: "instruction"
        instruction' 
            <- (Left <$> parseJSON instruct) <|> (Right <$> parseJSON instruct)
        pure $ MkInstruction aid instruction'