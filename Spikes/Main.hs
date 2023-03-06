module Main where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant.API
import Servant.HTML.Blaze
import Servant.Server
import Text.Blaze.Html
import Text.Hamlet

data Test = Test Int String
  deriving (Generic)

instance ToJSON Test

type TestAPI 
    =    "tests" :> Get '[JSON] [Test]
    :<|> "Test.html" :> Get '[HTML] Html

main :: IO ()
main = run 8080 (serve (Proxy :: Proxy TestAPI) serverTestAPI)

serverTestAPI :: Server TestAPI
serverTestAPI = tests :<|> testHtml

tests = return [ Test 1 "Test 1"
               , Test 2 "Test 2"
               ]

testHtml = return [shamlet|
  $doctype 5
    <html>
     <head>
       <title>This is a title
     <body>
       <p>This is text
  |]
