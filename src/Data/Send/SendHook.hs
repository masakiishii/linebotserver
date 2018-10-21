{-# LANGUAGE DeriveGeneric #-}

module Data.Send.SendHook where

import Data.Aeson
import GHC.Generics

import Data.Send.SendMessageData

data SendHook = SendHook
    {
      replyToken :: String
    , messages :: [SendMessageData]
    } deriving (Show, Generic)

instance FromJSON SendHook
instance ToJSON SendHook
