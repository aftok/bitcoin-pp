{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

-- import Options.Applicative

import Bippy
import qualified Bippy.Proto as P
import Bippy.Types
import Control.Monad.Except
import Crypto.PubKey.RSA.Types (Error (..))
import Crypto.Random.Types (MonadRandom, getRandomBytes)
import qualified Data.ByteString as B
import Data.Maybe (fromJust)
import Data.ProtocolBuffers (encodeMessage)
import Data.Serialize.Put
import Data.Time.Clock
import Data.X509
import Data.X509.File (readKeyFile, readSignedObject)
import Haskoin.Address (Address (..), addrToText, addressToOutput, textToAddr)
import Haskoin.Constants (btcTest)
import Haskoin.Util.Arbitrary.Crypto (arbitraryHash160)
import Test.QuickCheck

main :: IO ()
main = do
  hash <- generate arbitraryHash160
  putStrLn (show . addrToText btcTest . PubKeyAddress $ hash)

  either (putStrLn . show) pure =<< (runExceptT . runBippyM) writeSample1

newtype BippyM e a = BippyM {runBippyM :: ExceptT e IO a}
  deriving (Functor, Applicative, Monad)

instance MonadRandom (BippyM e) where
  getRandomBytes = BippyM . lift . getRandomBytes

instance MonadIO (BippyM e) where
  liftIO = BippyM . liftIO

instance MonadError e (BippyM e) where
  throwError = BippyM . throwError
  catchError b f = BippyM $ catchError (runBippyM b) (runBippyM . f)

writeSample1 :: BippyM Error ()
writeSample1 = do
  t <- liftIO $ getCurrentTime
  let paymentDetails = sample1 t
  liftIO $ B.writeFile "work/sample1.paymentdetails" . runPut $ encodeMessage paymentDetails

  -- load the private key
  privKeys <- liftIO $ readKeyFile "test-resources/ca/intermediate/private/aftok.bip70.key.pem"

  -- load the certificate chain
  pkiEntries <- liftIO $ readSignedObject "test-resources/ca/intermediate/certs/aftok.bip70-chain.cert.pem"

  -- generate payment request
  let privKey = case head privKeys of
        PrivKeyRSA k -> k
        _ -> error "Only RSA keys are supported for payment request signing."
      pkiData = X509SHA256 . CertificateChain $ pkiEntries

  paymentRequest <- BippyM . ExceptT $ createPaymentRequest privKey pkiData paymentDetails
  liftIO $ B.writeFile "work/sample1.bitcoinpaymentrequest" . runPut $ encodeMessage paymentRequest

-- write to payment request file

sourceAddr :: Address
sourceAddr = fromJust $ textToAddr btcTest "mmaFdFShk82G84DaccdqEvUCrMmSAAixJs"

recipient1 :: Address
recipient1 = fromJust $ textToAddr btcTest "mmBiyGP8TrX1erzEbX8jR5F56fLoxSX2Dr"

recipient2 :: Address
recipient2 = fromJust $ textToAddr btcTest "n4XB4L5rNpPD29CGDMfV6hcfLPPC13HkXV"

sample1 :: UTCTime -> P.PaymentDetails
sample1 sampleTime =
  createPaymentDetails
    btcTest
    [ Output (Satoshi 10000) (addressToOutput recipient1),
      Output (Satoshi 20000) (addressToOutput recipient2)
    ]
    sampleTime
    Nothing
    Nothing
    Nothing
    Nothing
