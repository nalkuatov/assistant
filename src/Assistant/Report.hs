module Assistant.Report where

import Universum hiding ((^.))
import Lens.Micro.Platform ((^.), at)
import Time (Time, sec)

import Assistant.TimeEntry

type ProjectId = Text

newtype Report = Report
  { entries :: HashMap ProjectId [TimeEntry]
  }

totalDuration :: ProjectId -> Report -> Duration
totalDuration project (Report { entries, .. })
  = (entries ^. at project) <&> mconcat . fmap duration
  ?: Duration $ sec 0
