module RequestBuilder where

import KeyReader
import Data.Position.CollateralData
   
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Lazy.Char8 as LBS
import Data.Time.Clock.POSIX (getPOSIXTime)
import Servant.API
import Servant.Client

import API.RequestApi
import Crypto.Hash.SHA256 (hmac)
import Data.Monoid ((<>))
    
createAccessSignature :: B.ByteString ->  B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
createAccessSignature secretAccessKey timeStamp httpMethod reqPath =
    Base16.encode $ hmac secretAccessKey (timeStamp <> httpMethod <> reqPath)

buildCollateralReq :: T.Text -> BS.ByteString -> IO (ClientM CollateralData)
buildCollateralReq accessKey secretKey = do
  unixTime <- getPOSIXTime
  let accessNonce = BS.pack . show . round $ unixTime
      httpMethod = BS.pack "GET"
      reqPath = BS.pack "/v1/me/getcollateral"
      accessNonce_T = TE.decodeUtf8 accessNonce
      sign_T = TE.decodeUtf8 $ createAccessSignature secretKey accessNonce httpMethod reqPath
  return $ getCollateral (Just accessKey) (Just accessNonce_T) (Just sign_T)