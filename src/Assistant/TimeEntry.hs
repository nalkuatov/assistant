module Assistant.TimeEntry where

import Data.Aeson (FromJSON(..))
import Universum
import GHC.Generics

import Time (Time, Second, RatioNat, sec)

newtype Duration = Duration (Time Second)
  deriving newtype (Semigroup, Monoid, Show)

instance FromJSON Duration where
  parseJSON v
    = asum [ fmap mkDuration $ parseJSON @Natural v
           , pure $ Duration $ sec 0
           ]
    where
      mkDuration = Duration . sec . fromIntegral @_ @RatioNat

data TimeEntry = TimeEntry
  { duration :: Duration
  , projectId :: Text
  }
  deriving stock Generic
  deriving anyclass FromJSON
