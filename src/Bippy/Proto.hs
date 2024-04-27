{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Bippy.Proto
  ( Output (..),
    PaymentDetails (..),
    PaymentRequest (..),
    X509Certificates (..),
    Payment (..),
    PaymentACK (..),
    defaultPaymentDetailsVersion,
  )
where

import Data.ByteString
import Data.ProtocolBuffers
import Data.Text
import Data.Word
import GHC.Generics (Generic)

data Output = Output
  { -- | integer-number-of-satoshis; use 0 as a default
    amount :: Optional 1 (Value Word64),
    -- | usually one of the standard Script forms
    script :: Required 2 (Value ByteString)
  }
  deriving (Generic, Eq, Show)

instance Encode Output

instance Decode Output

data PaymentDetails = PaymentDetails
  { -- | "main" or "test"; use "main" as a default
    network :: Optional 1 (Value Text),
    -- | Where payment should be sent
    outputs :: Repeated 2 (Message Output),
    -- | POSIX timestamp of payment request creation
    time :: Required 3 (Value Word64),
    -- | POSIX timestamp of when this request should be considered invalid
    expires :: Optional 4 (Value Word64),
    -- | Human-readable description of request for customer
    memo :: Optional 5 (Value Text),
    -- | URL which can receive Payment and return PaymentACK
    payment_url :: Optional 6 (Value Text),
    -- | Arbitrary data to include in the Payment message
    merchant_data :: Optional 7 (Value ByteString)
  }
  deriving (Generic, Eq, Show)

instance Encode PaymentDetails

instance Decode PaymentDetails

data PaymentRequest = PaymentRequest
  { -- | use 1 as a default
    payment_details_version :: Optional 1 (Value Word32),
    -- | none / x509+sha256 / x509+sha1
    pki_type :: Optional 2 (Value Text),
    -- | depends on pki_type
    pki_data :: Optional 3 (Value ByteString),
    -- | PaymentDetails
    serialized_payment_details :: Required 4 (Value ByteString),
    -- | pki-dependent signature
    signature :: Required 5 (Value ByteString)
  }
  deriving (Generic, Eq, Show)

defaultPaymentDetailsVersion :: Word32
defaultPaymentDetailsVersion = 1

instance Encode PaymentRequest

instance Decode PaymentRequest

data X509Certificates = X509Certificates
  { -- | DER-encoded X.509 certificate chain
    certificate :: Repeated 1 (Value ByteString)
  }
  deriving (Generic)

instance Encode X509Certificates

instance Decode X509Certificates

data Payment = Payment
  { -- | From PaymentDetails.merchant_data
    payment_merchant_data :: Optional 1 (Value ByteString),
    -- | Signed transactions to satisfy PaymentDetails.outputs
    transactions :: Repeated 2 (Value ByteString),
    -- | Where to send refunds, if a refund is necessary
    refund_to :: Repeated 3 (Message Output),
    -- | Human-readable message for the merchant
    payment_memo :: Optional 4 (Value Text)
  }
  deriving (Generic, Eq, Show)

instance Encode Payment

instance Decode Payment

data PaymentACK = PaymentACK
  { -- | Payment message that triggered this ACK
    payment :: Required 1 (Message Payment),
    -- | human-readable message for customer
    ack_memo :: Optional 2 (Value Text)
  }
  deriving (Generic, Eq, Show)

instance Encode PaymentACK

instance Decode PaymentACK
