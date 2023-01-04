module Assistant.Req
  ( fetchEntries
  , uploadEntries
  )
  where

import Universum
import Data.Aeson (Value)
import Network.HTTP.Req (MonadHttp, GET(..), https, req, NoReqBody(..), jsonResponse, ignoreResponse, responseBody, (/:))

import Assistant.Config
import Assistant.TimeEntry

fetchEntries :: (MonadHttp m, MonadReader Config m) => m [TimeEntry]
fetchEntries = do
  toggl <- asks _toggl
  resp  <- req GET (https toggl /: "entries") NoReqBody jsonResponse mempty
  pure $ responseBody resp

uploadEntries :: (MonadHttp m, MonadReader Config m) => m ()
uploadEntries = do
  tg <- asks _telegram
  void $ req GET (https tg /: "api") NoReqBody ignoreResponse mempty
