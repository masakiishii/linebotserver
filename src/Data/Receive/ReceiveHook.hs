{-# LANGUAGE DeriveGeneric #-}

module Data.Receive.ReceiveHook where

import Data.Aeson
import GHC.Generics

import Data.Receive.EventData

data ReceiveHook = ReceiveHook
    {
      events :: [EventData]
    } deriving (Show, Generic)

instance FromJSON ReceiveHook
instance ToJSON ReceiveHook
