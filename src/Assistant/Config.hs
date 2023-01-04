module Assistant.Config where

import Universum
import Control.Monad.Except (MonadError, throwError)
import Network.HTTP.Req (MonadHttp(handleHttpException), HttpException(..))

type Assistant = AssistantT IO

data AssistantError = NetworkError | DecodingError

newtype MonadIO m => AssistantT m a
  = AssistantT
  { unAssistantT :: ReaderT Config (ExceptT AssistantError m) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Config, MonadError AssistantError)

instance MonadHttp Assistant where
  handleHttpException = \case
    VanillaHttpException _ -> throwError NetworkError
    JsonHttpException _ -> throwError DecodingError

data Config
  = Config
  { _telegram :: Text -- telegram token
  , _toggl    :: Text -- toggltrack api token
  }
