module Domain.Agent where

data Agent =
    Agent
        { _resources :: Text
        , _userName :: Text 
        } deriving Show
