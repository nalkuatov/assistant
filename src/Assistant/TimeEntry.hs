module Assistant.TimeEntry where

import Universum
import Data.Aeson (FromJSON(..), fieldLabelModifier, defaultOptions)
import Data.Aeson.TH (deriveFromJSON)
import Data.Aeson.Casing (snakeCase)
import GHC.Generics (Generic)

import Time (Time, Second, RatioNat, sec)

newtype Duration = Duration (Time Second)
  deriving newtype (Semigroup, Monoid, Show)

instance FromJSON Duration where
  parseJSON v
    = asum [ fmap mkDuration $ parseJSON @Natural v
           , pure $ Duration $ sec 0
           ]
    where mkDuration = Duration . sec . fromIntegral @_ @RatioNat

data TimeEntry = TimeEntry
  { duration :: Duration
  , projectId :: Integer
  }
  deriving stock (Show, Generic)

$(deriveFromJSON defaultOptions { fieldLabelModifier = snakeCase } ''TimeEntry)
