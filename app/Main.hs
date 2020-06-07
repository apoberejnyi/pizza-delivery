module Main
  ( main
  )
where

import           Foundation                     ( startGateway )
import           LoadEnv
import           Prelude                        ( IO )

main :: IO ()
main = do
  loadEnv
  startGateway
