module Bippy.Tests (tests) where

import Test.Framework (Test, testGroup)

-- import Test.QuickCheck (Property, (==>))
-- import Test.Framework.Providers.QuickCheck2 (testProperty)

tests :: [Test]
tests =
  [ testGroup
      "creation of payment details"
      []
  ]

-- testProperty "serialization roundtrip" $ \(ArbitraryPaymentRequest x) -> checkRoundtrip x
