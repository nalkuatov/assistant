module Assistant.Req where

import Universum
import Data.Aeson (Value)
import Data.Time (UTCTime)
import Network.HTTP.Req (MonadHttp, GET(..), basicAuth, useHttpsURI, req, NoReqBody(..), jsonResponse, ignoreResponse, responseBody, (/:))

import Assistant.Config
import Assistant.Report
import Assistant.TimeEntry

fetchEntries :: UTCTime -> UTCTime -> Assistant Report
fetchEntries start end = do
  WebResource{..} <- asks toggl
  resp  <- req GET url NoReqBody jsonResponse (basicAuth username password)
  pure $ responseBody resp

uploadEntries :: Report -> Assistant ()
uploadEntries _ = do
  tg <- asks $ url . telegram
  void $ req GET tg NoReqBody ignoreResponse mempty
