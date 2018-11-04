{-# LANGUAGE OverloadedStrings #-}

module MMServer where

import Control.Monad.Reader
import Control.Concurrent
import Network.Wai.Handler.Warp
import Servant
import System.Process
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Data.Receive.ReceiveHook
import Data.Receive.ReceiveMessageData
import Data.Receive.EventData
import Data.Receive.SourceData
import Data.Send.SendHook
import Data.Send.SendMessageData
import Data.Send.PushData
import Data.Position.CollateralData
import API.RequestApi
import RequestBuilder
import KeyReader
import Executor

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

executeScript :: Maybe ReceiveMessageData -> IO ()
executeScript receiveMsg =
  case receiveMsg of
    Nothing -> return ()
    Just msg -> do
      let textMsg = Data.Receive.ReceiveMessageData.text msg
      executeScriptDone textMsg
      return ()

executeScriptDone :: String -> IO ()      
executeScriptDone msg
  | msg == "1" || msg == "start" = do
    _ <- forkIO $ do
      r <- createProcess (proc "./start-socket-io.sh" [])
      threadDelay (12 * 1000 * 1000)
      r1 <- createProcess (proc "./purified-trader-exe" ["--start"])
      return ()
    return ()
  | msg == "2" || msg == "stop" = do
    r <- createProcess (proc "./stop-socket-io.sh" [])
    return ()
  | msg == "3" || msg == "position" = return () -- TODO
  | otherwise = return () -- TODO

lineReceiveServer :: Manager -> T.Text -> Server LineReceiveAPI
lineReceiveServer manager lineToken = receiveMessage
  where receiveMessage :: Maybe T.Text -> ReceiveHook -> Handler NoContent
        receiveMessage signature receiveHook =
          do
            let evs = Data.Receive.ReceiveHook.events receiveHook
                evHead = head evs
                src = Data.Receive.EventData.source evHead
                message = Data.Receive.EventData.message evHead
            liftIO $ executeScript message
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
      userId = keys!!2
      accessKey = T.pack $ keys!!3
      secretKey = BS.pack $ keys!!4
  _ <- forkIO $ sendPosition manager lineToken userId accessKey secretKey
  run 8081 (lineApplication manager lineToken)

sendPosition :: Manager -> T.Text -> String -> T.Text -> BS.ByteString -> IO ()
sendPosition manager lineToken userId accessKey secretKey =
  flip fix (0 :: Int) $ \loop i ->
  when True $ do
    req <- buildCollateralReq accessKey secretKey
    ret <- executeBit manager $ req
    case ret of
      Left x-> do loop $ i
      Right x -> do
        colBase <- readBaseCollateral
        let col = Data.Position.CollateralData.collateral x
            pl = col - colBase
            textMsg = "Today's PL is: " ++ show (pl)
            msg = SendMessageData { Data.Send.SendMessageData.typeString = "text", Data.Send.SendMessageData.text = textMsg }
            body = PushData { toUserId = userId, Data.Send.PushData.messages = [msg] }
        res <- execute manager $ pushMessage (Just "application/json") (Just lineToken) body
        threadDelay (5 * 60 * 1000 * 1000)
        loop $ i

readBaseCollateral :: IO Integer
readBaseCollateral = do
  col <- readFile "baseCollateral.txt"
  return $ (read col :: Integer)