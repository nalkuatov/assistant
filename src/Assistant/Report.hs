module Assistant.Report where

import Universum hiding ((^.))
import qualified Data.HashMap.Strict as HM
import Data.Aeson (FromJSON(..))
import Lens.Micro.Platform ((^.), at)
import Time (Time, sec)

import Assistant.TimeEntry

type ProjectId = Integer

newtype Report = Report (HashMap ProjectId Duration)
  deriving stock Show

instance FromJSON Report where
  parseJSON v =
    let toPair TimeEntry{..} = (projectId, duration)
    in (Report . fromList . fmap toPair) <$> parseJSON @[TimeEntry] v

instance FromList Report where
  type ListElement Report = (ProjectId, Duration)
  fromList = Report . HM.fromListWith (<>)
