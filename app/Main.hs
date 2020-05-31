module Main (main) where

import Foundation ( startGateway )
import LoadEnv
import Persistence.PG
import Prelude ( IO )

main :: IO ()
main = do
    loadEnv
    checkPGEnv
    startGateway
