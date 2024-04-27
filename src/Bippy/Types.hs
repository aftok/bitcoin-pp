{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Bippy.Types where

import qualified Bippy.Proto as P
import Control.Lens (makePrisms)
import Data.ByteString
import Data.ProtocolBuffers
import Data.Serialize.Get (runGet)
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import Data.X509
import Haskoin.Script (ScriptOutput, encodeOutputBS)

newtype Satoshi = Satoshi Word64 deriving (Eq, Enum, Ord, Show)

makePrisms ''Satoshi

instance Semigroup Satoshi where
  (Satoshi a) <> (Satoshi b) = Satoshi (a + b)

instance Monoid Satoshi where
  mempty = Satoshi 0

data PKIData
  = None
  | X509SHA256 CertificateChain
  | X509SHA1 CertificateChain

pkiName :: PKIData -> Text
pkiName None = "none"
pkiName (X509SHA256 _) = "x509+sha256"
pkiName (X509SHA1 _) = "x509+sha1"

pkiCertChain :: PKIData -> Maybe CertificateChain
pkiCertChain None = Nothing
pkiCertChain (X509SHA256 certs) = Just certs
pkiCertChain (X509SHA1 certs) = Just certs

data Output = Output
  { amount :: Satoshi,
    script :: ScriptOutput
  }
  deriving (Eq)

outputProto :: Output -> P.Output
outputProto (Output (Satoshi a) s) =
  P.Output
    { P.amount = putField $ Just a,
      P.script = putField $ encodeOutputBS s
    }

x509CertificatesProto :: CertificateChain -> P.X509Certificates
x509CertificatesProto certs =
  let (CertificateChainRaw encCerts) = encodeCertificateChain certs
   in P.X509Certificates
        { P.certificate = putField encCerts
        }

newtype Expiry = Expiry {expiryTime :: UTCTime}

getPaymentDetails :: P.PaymentRequest -> Either String P.PaymentDetails
getPaymentDetails req =
  let serialized_payment_bytes :: ByteString
      serialized_payment_bytes = getField $ P.serialized_payment_details req
   in runGet decodeMessage serialized_payment_bytes

getExpires :: P.PaymentDetails -> Maybe Expiry
getExpires d =
  (Expiry . posixSecondsToUTCTime . fromIntegral) <$> getField (P.expires d)
