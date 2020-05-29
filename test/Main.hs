module Main where

import qualified Spec.OrderOption as OrderOption
import Test.Hspec

main :: IO ()
main = hspec OrderOption.spec
