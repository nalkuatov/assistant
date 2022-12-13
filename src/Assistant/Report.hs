module Assistant.Report where

import Universum
import qualified Data.HashMap.Strict as Map
import Time (Time, sec, (+:+))

import Assistant.TimeEntry

type ProjectId = Text

newtype Report timeunit = Report
  { entries :: HashMap ProjectId [TimeEntry timeunit]
  }

totalDuration :: ProjectId -> Report timeunit -> Time timeunit
totalDuration project (Report { entries, ..}) = fromMaybe (sec 0) $
  (foldl (+:+) (sec 0)) <$> Map.lookup project entries
