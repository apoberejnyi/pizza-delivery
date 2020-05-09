module Main where

import           Lib

main :: IO ()
main = someFunc

data Order = Order {
  items:: [String   ],
  address:: String
}
