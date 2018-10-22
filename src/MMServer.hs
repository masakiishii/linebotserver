{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module MMServer where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Servant
import Servant.Client
import System.Directory
import qualified Data.Aeson.Parser

import qualified Data.Text as T
import Data.Receive.ReceiveHook
import Data.Receive.ReceiveMessageData
import Data.Receive.EventData
import Data.Receive.SourceData
import Data.Send.SendHook
import Data.Send.SendMessageData

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import KeyReader
import Executor

import System.Process

type LineReceiveAPI = "webhook"
                :> Header "X-Line-Signature" T.Text
                :> ReqBody '[JSON] ReceiveHook
                :> Post '[JSON] NoContent

type LineSendAPI = "v2" :> "bot" :> "message" :> "reply"
                :> Header "Content-Type" T.Text
                :> Header "Authorization" T.Text
                :> ReqBody '[JSON] SendHook
                :> Post '[JSON] NoContent

lineSendApi :: Proxy LineSendAPI
lineSendApi = Proxy

putMessage :: Maybe T.Text -> Maybe T.Text -> SendHook ->  ClientM NoContent
putMessage = client lineSendApi

makeBody :: ReceiveHook -> SendHook
makeBody receiveHook =
  SendHook { Data.Send.SendHook.replyToken = rToken, Data.Send.SendHook.messages = msges }
  where
    headEvent = head $ Data.Receive.ReceiveHook.events receiveHook
    rToken = Data.Receive.EventData.replyToken headEvent
    messageData = Data.Receive.EventData.message headEvent
    msges = case messageData of
      Nothing -> [SendMessageData { Data.Send.SendMessageData.typeString = "text", Data.Send.SendMessageData.text = "hi, user! May I help you?" }]
      Just x -> let m = Data.Receive.ReceiveMessageData.text x
                in [SendMessageData { Data.Send.SendMessageData.typeString = "text", Data.Send.SendMessageData.text = makeMessage m }]

makeMessage :: String -> String
makeMessage msg
  | msg == "1" || msg == "start" = "Sure!, Let's execute M&Ms!"
  | msg == "2" || msg == "stop" = "It's Okey!, try to stop M&Ms..."
  | msg == "3" || msg == "position" = "Sure!, let me see... Here!, it's your P&L!"
  | otherwise = "hi, user! May I help you?"

executeReply :: Manager -> T.Text -> ReceiveHook -> IO ()
executeReply manager lineToken receiveHook = do
  print "reply message"
  let body = makeBody receiveHook
  print body
  res <- execute manager $ putMessage (Just "application/json") (Just lineToken) body
  case res of
    Left e -> print e
    Right t -> print t

lineReceiveServer :: Manager -> T.Text -> Server LineReceiveAPI
lineReceiveServer manager lineToken = receiveMessage
  where receiveMessage :: Maybe T.Text -> ReceiveHook -> Handler NoContent
        receiveMessage signature receiveHook =
          do
            let evs = Data.Receive.ReceiveHook.events receiveHook
                evHead = head evs
                src = Data.Receive.EventData.source evHead
            liftIO $ print $ length evs
            liftIO $ print lineToken
            liftIO $ print $ Data.Receive.EventData.replyToken evHead
            liftIO $ print $ Data.Receive.EventData.typeString evHead
            liftIO $ print $ Data.Receive.EventData.timestamp evHead
            liftIO $ executeReply manager lineToken receiveHook
            return NoContent

lineReceiveApi :: Proxy LineReceiveAPI
lineReceiveApi = Proxy

lineApplication :: Manager -> T.Text -> Application
lineApplication manager lineToken = serve lineReceiveApi (lineReceiveServer manager lineToken)

bootServer :: IO ()
bootServer = do
  csvData <- BL.readFile "./key-file.csv"
  manager <- newManager tlsManagerSettings
  let keys = readKeyData csvData
      lineToken = T.pack $ "Bearer " ++ keys!!0
      channelSecretKey = T.pack $ keys!!1
  run 8081 (lineApplication manager lineToken)

