module Main where

import qualified Spec.Order                    as Order
import qualified Spec.OrderOption              as OrderOption
import           Test.Hspec

main :: IO ()
main = hspec $ do
  Order.spec
  OrderOption.spec
