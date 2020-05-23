module Main (main) where

import Base.PG
import Foundation (startGateway)
import LoadEnv
import Prelude (IO)

main :: IO ()
main = do
    loadEnv
    checkPGEnv
    startGateway
