module Main where

import qualified Bippy.Tests (tests)
import Test.Framework (defaultMain)

main :: IO ()
main = defaultMain Bippy.Tests.tests
