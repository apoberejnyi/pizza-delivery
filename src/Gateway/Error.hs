{-# LANGUAGE OverloadedStrings #-}

module Gateway.Error where

import Control.Error
import Data.Aeson hiding ( json )
import Data.Aeson.Extra.Merge
import Network.HTTP.Types
import Web.Scotty.Trans

httpError :: (Monad m, ScottyError e, Error a) => Status -> a -> ActionT e m ()
httpError httpStatus err = status httpStatus >> json payload
        where
    payload = maybe corePayload (lodashMerge corePayload) (details err)
    corePayload = object
        [ "code" .= code err
        , "message" .= show err
        ]

