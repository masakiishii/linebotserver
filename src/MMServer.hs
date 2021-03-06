{-# LANGUAGE OverloadedStrings #-}

module MMServer where

import Control.Monad.Reader
import Control.Concurrent
import Network.Wai.Handler.Warp
import Servant
import System.Process
import System.Process.Internals
import qualified System.Posix.Signals as PS
import System.IO
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

makeBody :: ReceiveHook -> String -> SendHook
makeBody receiveHook msg =
  SendHook { Data.Send.SendHook.replyToken = rToken, Data.Send.SendHook.messages = msges }
  where
    headEvent = head $ Data.Receive.ReceiveHook.events receiveHook
    rToken = Data.Receive.EventData.replyToken headEvent
    messageData = Data.Receive.EventData.message headEvent
    msges = case messageData of
      Nothing -> [SendMessageData { Data.Send.SendMessageData.typeString = "text", Data.Send.SendMessageData.text = "hi, user! May I help you?" }]
      Just x -> let m = Data.Receive.ReceiveMessageData.text x
                in [SendMessageData { Data.Send.SendMessageData.typeString = "text", Data.Send.SendMessageData.text = msg }]
                
executeReply :: Manager -> T.Text -> ReceiveHook -> String -> IO ()
executeReply manager lineToken receiveHook msg = do
  print "reply message"
  let body = makeBody receiveHook msg
  print body
  res <- execute manager $ putMessage (Just "application/json") (Just lineToken) body
  case res of
    Left e -> print e
    Right t -> print t

executeScript :: Maybe ReceiveMessageData -> Manager -> T.Text -> BS.ByteString -> IO String
executeScript receiveMsg manager accessKey secretKey =
  case receiveMsg of
    Nothing -> return $ "Error occured!"
    Just msg -> do
      let textMsg = Data.Receive.ReceiveMessageData.text msg
      executeScriptDone textMsg manager accessKey secretKey

executeScriptDone :: String -> Manager -> T.Text -> BS.ByteString -> IO String
executeScriptDone msg manager accessKey secretKey
  | msg == "1" || msg == "start" = do
    _ <- forkIO $ do
      r <- createProcess (proc "./start-trader.sh" [])
      return ()
    return $ "Sure!, Let's execute M&Ms!"
  | msg == "2" || msg == "stop" = do
    r <- createProcess (proc "./stop-trader.sh" [])
    return $ "It's Okey!, try to stop M&Ms..."
  | msg == "3" || msg == "position" = do
    req <- buildCollateralReq accessKey secretKey
    ret <- executeBit manager $ req
    case ret of
      Left x-> return $ "Error! Please Retry!" ++ show(x)
      Right x -> do
        colBase <- readBaseCollateral
        let col = Data.Position.CollateralData.collateral x
            openpl = Data.Position.CollateralData.openPositionPnL x
            pl = col - colBase
        return $ "Sure!, let me see... Here!, it's your P&L! " ++ "Today's PL is: " ++ show (pl) ++ ", and Open position PL is: " ++ show(openpl)
  | msg == "4" = do
    _ <- forkIO $ do
      r <- createProcess (proc "./start-feeder.sh" [])
      return ()
    return $ "Sure!, Let's start feeder!"
  | msg == "5" = do
    r <- createProcess (proc "./stop-feeder.sh" [])
    return $ "It's Okey!, try to stop feeder..."
  | msg == "6" = checkProcess "./check-trade-process.sh"
  | msg == "7" = checkProcess "./check-feed-process.sh"    
  | otherwise = return $ unlines [
    "hi, user! May I help you?",
    "1: start trade",
    "2: stop  trade",
    "3: check position PL",
    "4: start feeder",
    "5: stop  feeder",
    "6: check trade process",
    "7: check feed  process",
    "otherwise: menu"
    ]

lineReceiveApi :: Proxy LineReceiveAPI
lineReceiveApi = Proxy

lineApplication :: Manager -> T.Text -> T.Text -> BS.ByteString -> Application
lineApplication manager lineToken accessKey secretKey = serve lineReceiveApi (lineReceiveServer manager lineToken accessKey secretKey)

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
  run 8081 (lineApplication manager lineToken accessKey secretKey)

sendPosition :: Manager -> T.Text -> String -> T.Text -> BS.ByteString -> IO ()
sendPosition manager lineToken userId accessKey secretKey =
  flip fix (0 :: Int) $ \loop i ->
  when True $ do
    threadDelay (120 * 60 * 1000 * 1000)
    req <- buildCollateralReq accessKey secretKey
    ret <- executeBit manager $ req
    case ret of
      Left x-> do loop $ i
      Right x -> do
        colBase <- readBaseCollateral
        feederProcess <- checkProcess "./check-feed-process.sh"
        tradeProcess <- checkProcess "./check-trade-process.sh"
        let col = Data.Position.CollateralData.collateral x
            pl = col - colBase
            textMsg = "Today's PL is: " ++ show (pl)
            msg1 = SendMessageData { Data.Send.SendMessageData.typeString = "text", Data.Send.SendMessageData.text = textMsg }
            msg2 = SendMessageData { Data.Send.SendMessageData.typeString = "text", Data.Send.SendMessageData.text = feederProcess }
            msg3 = SendMessageData { Data.Send.SendMessageData.typeString = "text", Data.Send.SendMessageData.text = tradeProcess }
            body = PushData { toUserId = userId, Data.Send.PushData.messages = [msg1, msg2, msg3] }
        res <- execute manager $ pushMessage (Just "application/json") (Just lineToken) body
        loop $ i

checkProcess :: String -> IO String
checkProcess shellPath = do
  (_, Just hout, _, handle) <- createProcess (proc shellPath []) { std_out = CreatePipe }
  exitCode <- waitForProcess handle
  result <- hGetContents hout
  withProcessHandle handle $ \ph_ ->
    case ph_ of
      OpenHandle pid -> PS.signalProcess PS.sigKILL pid
      ClosedHandle _ -> return () 
  return $ filter (/= '\n') result

readBaseCollateral :: IO Integer
readBaseCollateral = do
  col <- readFile "baseCollateral.txt"
  return $ (read col :: Integer)

lineReceiveServer :: Manager -> T.Text -> T.Text -> BS.ByteString -> Server LineReceiveAPI
lineReceiveServer manager lineToken accessKey secretKey = receiveMessage
  where receiveMessage :: Maybe T.Text -> ReceiveHook -> Handler NoContent
        receiveMessage signature receiveHook =
          do
            let evs = Data.Receive.ReceiveHook.events receiveHook
                evHead = head evs
                src = Data.Receive.EventData.source evHead
                message = Data.Receive.EventData.message evHead
            msg <- liftIO $ executeScript message manager accessKey secretKey
            liftIO $ executeReply manager lineToken receiveHook msg
            return NoContent
