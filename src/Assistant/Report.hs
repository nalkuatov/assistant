module Assistant.Report where

import Universum hiding ((^.))
import Lens.Micro.Platform ((^.), at)
import Time (Time, sec, (+:+), time)

import Assistant.TimeEntry

type ProjectId = Text

newtype Report timeunit = Report
  { entries :: HashMap ProjectId [TimeEntry timeunit]
  }

totalDuration :: forall timeunit. ProjectId -> Report timeunit -> Time timeunit
totalDuration project (Report { entries, .. })
  = (entries ^. at project) <&> mconcat . fmap duration
  ?: time @timeunit 0
