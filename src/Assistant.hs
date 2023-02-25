module Assistant
  (module Assistant.Report)
  where

import Universum
import Time (threadDelay, day, sec)
import Data.Time (UTCTime(..), getCurrentTime)
import Data.Yaml (decodeFileThrow)
import qualified Data.Conduit as Conduit (runConduit, yieldM, awaitForever)
import Data.Conduit (ConduitT, Source, (.|))
import qualified Data.Conduit.Combinators as Conduit (repeatM, sinkNull, mapM, print)

import Assistant.Config
import Assistant.TimeEntry
import Assistant.Report
import Assistant.Req

run :: IO ()
run = do
  now <- getCurrentTime
  config <- decodeFileThrow "config.yaml"
  runAssistant config $ Conduit.runConduit assistantConduit

assistantConduit :: ConduitT () Void Assistant ()
assistantConduit = entries .| Conduit.print
  where
    entries = do
      now <- liftIO getCurrentTime
      Conduit.repeatM $ fetchEntries now now
