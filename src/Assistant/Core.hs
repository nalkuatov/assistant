module Assistant.Core where

import Universum

import Time (Time)
import Data.Time.Clock.POSIX (POSIXTime)

data TimeEntry timeunit = TimeEntry
  { duration :: Time timeunit
  , descr :: Text
  }