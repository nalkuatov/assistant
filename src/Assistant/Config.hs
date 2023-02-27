module Assistant.Config where

import Universum
import Fmt (Buildable(..))
import Text.URI (URI, mkURI)
import Data.Aeson (FromJSON(..), (.:), Value(..))
import Network.HTTP.Client (responseStatus, getOriginalRequest, host)
import Network.HTTP.Types (statusCode, Status)
import Control.Monad.Except (MonadError, throwError)
import Network.HTTP.Req (MonadHttp(handleHttpException), isStatusCodeException, HttpException(..), Url, Scheme(..), useHttpsURI)
import Text.Interpolation.Nyan

type Assistant = AssistantT IO AssistantError

data AssistantError
  = ServerError ByteString Status
  | NetworkError Text
  | DecodingError Text
  deriving stock Show
  deriving anyclass Exception

instance Buildable AssistantError where
  build = \case
    ServerError (decodeUtf8 @Text -> host) status ->
      [int||Server #{host} responded with error #{statusCode status}|]
    NetworkError e -> [int||e|]
    DecodingError e -> [int||Failed to decode response: #{e}|]

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
          let h = host $ getOriginalRequest resp
              s = responseStatus resp
          in throwError $ ServerError h s
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
  { telegram :: WebResource
  , toggl    :: WebResource
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

data WebResource
  = WebResource
  { url :: Address 'Https
  , username :: Text
  , password :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass FromJSON

newtype Address (scheme :: Scheme) = Address (Url scheme)
 deriving stock (Show, Eq)

instance FromJSON (Address 'Https) where
  parseJSON v = do
    text <- parseJSON v
    case parseUrl text of
      Just url -> pure $ Address url
      _ -> empty
    where
      parseUrl = fmap fst . useHttpsURI <=< mkURI
