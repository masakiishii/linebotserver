{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.RequestApi where

import Servant.Client
import Servant.API
import Data.Proxy

import qualified Data.Text as T

import Data.Position.CollateralData
import Data.Receive.ReceiveHook
import Data.Send.SendHook
import Data.Send.PushData

type LineReceiveAPI = "webhook"
                :> Header "X-Line-Signature" T.Text
                :> ReqBody '[JSON] ReceiveHook
                :> Post '[JSON] NoContent

type ClientAPI = "v2" :> "bot" :> "message" :> "reply"
                :> Header "Content-Type" T.Text
                :> Header "Authorization" T.Text
                :> ReqBody '[JSON] SendHook
                :> Post '[JSON] NoContent

                :<|> "v2" :> "bot" :> "message" :> "push"
                :> Header "Content-Type" T.Text
                :> Header "Authorization" T.Text
                :> ReqBody '[JSON] PushData
                :> Post '[JSON] NoContent

                :<|> "v1" :> "me" :> "getcollateral"
                :> Header "ACCESS-KEY" T.Text
                :> Header "ACCESS-TIMESTAMP" T.Text
                :> Header "ACCESS-SIGN" T.Text
                :> Get '[JSON] CollateralData      

lineSendApi :: Proxy ClientAPI
lineSendApi = Proxy

putMessage :: Maybe T.Text -> Maybe T.Text -> SendHook -> ClientM NoContent
pushMessage :: Maybe T.Text -> Maybe T.Text -> PushData -> ClientM NoContent
getCollateral :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> ClientM CollateralData

putMessage :<|> pushMessage :<|> getCollateral = client lineSendApi