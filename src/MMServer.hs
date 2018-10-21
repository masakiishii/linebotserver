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


data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving Generic

instance ToJSON Position

-- [API] -----------------------------------------------------------------------------------------------
type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      -- :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] NoContent

type LineReceiveAPI = "webhook"
                :> Header "X-Line-Signature" T.Text
                :> ReqBody '[JSON] ReceiveHook
                :> Post '[JSON] NoContent

type LineSendAPI = "v2" :> "bot" :> "message" :> "reply"
                :> Header "Content-Type" T.Text
                :> Header "Authorization" T.Text
                :> ReqBody '[JSON] SendHook
                :> Post '[JSON] NoContent
--------------------------------------------------------------------------------------------------------


newtype HelloMessage = HelloMessage { msg :: String }
  deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving Generic

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'

  where from'    = "great@company.com"
        to'      = clientEmail c
        subject' = "Hey " ++ clientName c ++ ", we miss you!"
        body'    = "Hi " ++ clientName c ++ ",\n\n"
                ++ "Since you've recently turned " ++ show (clientAge c)
                ++ ", have you checked out our latest "
                ++ intercalate ", " (clientInterestedIn c)
                ++ " products? Give us a visit!"

server3 :: Int -> Server API
server3 num = position
     :<|> hello
     :<|> marketing

  where position :: Int -> Int -> Handler Position
        position x y = return (Position num y)

        hello :: Maybe String -> Handler HelloMessage
        hello mname = return . HelloMessage $ case mname of
          Nothing -> "Hello, anonymous coward"
          Just n  -> "Hello, " ++ n

        marketing :: ClientInfo -> Handler NoContent
        -- marketing clientinfo = return (emailForClient clientinfo)
        marketing clientinfo = return NoContent

-----------------------------------------------------------------
lineSendApi :: Proxy LineSendAPI
lineSendApi = Proxy

putMessage :: Maybe T.Text -> Maybe T.Text -> SendHook ->  ClientM NoContent
putMessage = client lineSendApi
-----------------------------------------------------------------

makeBody :: ReceiveHook -> SendHook
makeBody receiveHook =
  SendHook { Data.Send.SendHook.replyToken = rToken, Data.Send.SendHook.messages = msges }
  where
    rToken = Data.Receive.EventData.replyToken (head $ Data.Receive.ReceiveHook.events receiveHook)
    msges = [SendMessageData { Data.Send.SendMessageData.typeString = "text", Data.Send.SendMessageData.text = "hi, user! May I help you?" }]

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
            -- let a = execute manager $! putMessage (Just "application/json") (Just lineToken) (makeBody receiveHook)
            liftIO $ executeReply manager lineToken receiveHook
            return NoContent

userAPI3 :: Proxy API
userAPI3 = Proxy

lineReceiveApi :: Proxy LineReceiveAPI
lineReceiveApi = Proxy

app3 :: Int -> Application
app3 num = serve userAPI3 (server3 num)

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
  -- func manager accessKey secretKey lineToken
  -- run 8081 (app3 3)

