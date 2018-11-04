{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Send.PushData where

import Data.Send.SendMessageData

import Data.Aeson
import GHC.Generics

data PushData = PushData
    {
      toUserId :: String
    , messages :: [SendMessageData]
    } deriving (Show, Generic)

instance FromJSON PushData where
  parseJSON (Object o) =
    PushData <$> o .: "to"
             <*> o .: "messages"

instance ToJSON PushData where
  toJSON v = object ["to" .= toUserId v,
                     "messages" .= messages v]
