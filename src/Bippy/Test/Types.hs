module Bippy.Test.Types
  ( arbitrarySatoshi,
    arbitraryOutput,
  )
where

import Bippy.Types (Output (..), Satoshi (..))
import qualified Haskoin.Constants as H
import qualified Haskoin.Util.Arbitrary.Script as H
import qualified Haskoin.Util.Arbitrary.Transaction as H
import Test.QuickCheck (Gen)

arbitrarySatoshi :: H.Network -> Gen Satoshi
arbitrarySatoshi = fmap (Satoshi . H.getTestCoin) . H.arbitrarySatoshi

arbitraryOutput :: H.Network -> Gen Output
arbitraryOutput n = Output <$> arbitrarySatoshi n <*> H.arbitraryScriptOutput n
