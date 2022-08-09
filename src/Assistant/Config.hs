module Assistant.Config where

import Universum

import Time (Time)

data Config timeunit = Config
  { telegram :: String
  , toggl    :: String
  , cooldown :: Time timeunit
  }