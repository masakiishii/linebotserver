module Executor where

import Network.HTTP.Client (Manager)
import Servant.Client

execute :: Manager -> ClientM a -> IO (Either ClientError a)
execute manager client = runClientM client $ mkClientEnv manager $ BaseUrl Https "api.line.me" 443 ""

executeBit :: Manager -> ClientM a -> IO (Either ClientError a)
executeBit manager client = runClientM client $ mkClientEnv manager $ BaseUrl Https "api.bitflyer.com" 443 ""
