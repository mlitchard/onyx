module Main where

-- import Main.Utf8 qualified as Utf8
import           Network.Wai.Handler.Warp (run)
import AppServer 

{- |
 Main entry point.

 The `, run` script will invoke this function.
-}
main :: IO ()
main = run 8080 app 

