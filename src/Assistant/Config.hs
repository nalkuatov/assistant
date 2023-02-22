module Assistant.Config where

import Universum
import Text.URI (URI)
import Network.HTTP.Client (responseStatus, getOriginalRequest, host)
import Control.Monad.Except (MonadError, throwError)
import Network.HTTP.Req (MonadHttp(handleHttpException), isStatusCodeException, HttpException(..), Url, Scheme(..))

type Assistant = AssistantT IO AssistantError

data AssistantError = ServerError ByteString | NetworkError Text | DecodingError Text
  deriving stock Show
  deriving anyclass Exception

newtype MonadIO m => AssistantT m e a
  = AssistantT
  { unAssistantT :: ReaderT Config (ExceptT AssistantError m) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Config, MonadError AssistantError)

instance MonadHttp Assistant where
  handleHttpException = \case
    e@(VanillaHttpException _) ->
      case isStatusCodeException e of
        Just(resp) ->
          let msg = (host $ getOriginalRequest resp) <> " " <> (show $ responseStatus resp)
          in throwError $ ServerError msg
        _ -> throwError $ NetworkError $ show e
    JsonHttpException e -> throwError $ DecodingError $ show e

runAssistant :: Config -> Assistant a -> IO a
runAssistant config = (>>= res) . runExceptT . ($ config) . runReaderT . unAssistantT
  where
    res = \case
      Right a -> pure a
      Left e  -> throwM e

data Config
  = Config
  { telegram :: WebResource Https
  , toggl    :: WebResource Https
  }

data WebResource scheme
  = WebResource
  { url :: Url scheme
  , username :: ByteString
  , password :: ByteString
  }
  deriving stock (Show, Eq)
