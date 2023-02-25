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
  WebResource{url = Address url, ..} <- asks toggl
  resp <- req GET url NoReqBody jsonResponse
      (basicAuth (encodeUtf8 username) (encodeUtf8 password))
  pure $ responseBody resp

uploadEntries :: Report -> Assistant ()
uploadEntries _ = do
  WebResource{url = Address url, ..} <- asks telegram
  void $ req GET url NoReqBody ignoreResponse mempty
