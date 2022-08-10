module Assistant.Notify where

import Universum

import Control.Monad.Except (ExceptT)

import Assistant.Core

type Secret = Text

data Target
  = Telegram Secret

data NotifyError
  = InvalidTargetUrl Target

type Notify timeunit a = [TimeEntry timeunit] -> ExceptT NotifyError IO a

formNotifiers :: [Target] -> [TimeEntry timeunit] -> [Notify timeunit a]
formNotifiers targets entries = undefined
