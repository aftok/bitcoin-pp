{-# LANGUAGE FlexibleContexts #-}

module Bippy
  ( createPaymentDetails,
    unsignedPaymentRequest,
    createPaymentRequest,
  )
where

-- import Data.X509

import qualified Bippy.Proto as P
import Bippy.Types
import Crypto.Hash.Algorithms (SHA1 (..), SHA256 (..))
import Crypto.PubKey.RSA.PKCS15 (signSafer)
import Crypto.PubKey.RSA.Types (PrivateKey)
import qualified Crypto.PubKey.RSA.Types as C (Error (..))
import Crypto.Random.Types (MonadRandom)
import Data.ByteString (ByteString, empty)
import Data.ProtocolBuffers
import Data.Serialize.Put
import Data.Text (Text, pack)
import Data.Time.Clock
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Haskoin.Constants (Network, getNetworkName)
import Network.URI

createPaymentDetails ::
  -- | bitcoin network for which the payment request is to be created
  Network ->
  -- | set of outputs being requested
  [Output] ->
  -- | creation time of payment request
  UTCTime ->
  -- | optional time at which the request will expire
  Maybe Expiry ->
  -- | arbitrary memo to be added to the payment request
  Maybe Text ->
  -- | URL to which a Payment message may be sent for acknowledgement
  Maybe URI ->
  -- | arbitrary merchant payload
  Maybe ByteString ->
  -- | Returns the Protocol Buffer containing payment details.
  P.PaymentDetails
createPaymentDetails network outputs time expiry memo payment_url merchant_data =
  P.PaymentDetails
    { P.network = putField . Just . pack $ getNetworkName network,
      P.outputs = putField $ fmap outputProto outputs,
      P.time = putField $ posixSeconds time,
      P.expires = putField $ posixSeconds . expiryTime <$> expiry,
      P.memo = putField memo,
      P.payment_url = putField $ pack . show <$> payment_url,
      P.merchant_data = putField merchant_data
    }
  where
    posixSeconds = round . utcTimeToPOSIXSeconds

unsignedPaymentRequest ::
  -- | certificate chain to be used to sign the request
  PKIData ->
  -- | Payment details to be signed
  P.PaymentDetails ->
  -- | Returns the unsignemd payment request
  P.PaymentRequest
unsignedPaymentRequest pkid details =
  P.PaymentRequest
    { P.payment_details_version = putField $ Just P.defaultPaymentDetailsVersion,
      P.pki_type = putField . Just $ pkiName pkid,
      P.pki_data = putField $ fmap (runPut . encodeMessage . x509CertificatesProto) (pkiCertChain pkid),
      P.serialized_payment_details = putField . runPut $ encodeMessage details,
      P.signature = putField empty
    }

createPaymentRequest ::
  (MonadRandom m) =>
  -- | private key to be used to sign the request - must correspond to head of the cert chain
  PrivateKey ->
  -- | certificate chain to be used to sign the request
  PKIData ->
  -- | Payment details to be signed
  P.PaymentDetails ->
  m (Either C.Error P.PaymentRequest)
createPaymentRequest key pkid details =
  let unsignedReq = unsignedPaymentRequest pkid details
      serializedUnsignedRequest = runPut $ encodeMessage unsignedReq
      req s =
        P.PaymentRequest
          { P.payment_details_version = P.payment_details_version unsignedReq,
            P.pki_type = P.pki_type unsignedReq,
            P.pki_data = P.pki_data unsignedReq,
            P.serialized_payment_details = P.serialized_payment_details unsignedReq,
            P.signature = putField s
          }
      signf (X509SHA256 _) = signSafer (Just SHA256)
      signf (X509SHA1 _) = signSafer (Just SHA1)
      signf None = \_ _ -> pure $ Left C.InvalidParameters
   in fmap req <$> signf pkid key serializedUnsignedRequest
