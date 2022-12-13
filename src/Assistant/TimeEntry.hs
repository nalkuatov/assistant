module Assistant.TimeEntry where

import Universum

import Time (Time)

data TimeEntry timeunit = TimeEntry
  { duration :: Time timeunit
  , descr :: Text
  , projectId :: Text
  }