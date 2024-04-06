
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Http.Handler (
    Handler
  , application
  , middleware
  , execHandler
  , getRequest
  , getResponse
  , setStatus
  , setHeader
  , getMethod
  , getHeaders
  , text
) where

import           Control.Exception      (SomeException (..), catch)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Transformer
import           Data.Either            (fromLeft)
import           Data.EitherT
import           Data.StateT
import           Data.Text              (Text)
import           Data.Text.Encoding     (encodeUtf8Builder)
import           Network.HTTP.Types
import           Network.Wai


type Context = (Request, Response)

newtype HandlerM m a = HandlerM {
  runHandlerM :: EitherT (IO Response) (StateT Context m) a
  }

instance Monad m => Functor (HandlerM m) where
  fmap :: (a -> b) -> HandlerM m a -> HandlerM m b
  fmap fn (HandlerM e) = HandlerM $ fn <$> e

instance Monad m => Applicative (HandlerM m) where
  pure :: a -> HandlerM m a
  pure = HandlerM . return

  (<*>) :: HandlerM m (a -> b) -> HandlerM m a -> HandlerM m b
  (HandlerM fn) <*> (HandlerM e) = HandlerM $ fn <*> e

instance Monad m => Monad (HandlerM m) where
  return :: a -> HandlerM m a
  return = pure

  (>>=) :: HandlerM m a -> (a -> HandlerM m b) -> HandlerM m b
  (HandlerM e) >>= fn = HandlerM $ e >>= runHandlerM . fn

instance  MonadTrans HandlerM where
  lift :: Monad m => m a -> HandlerM m a
  lift = HandlerM . lift . lift

instance (Monad m, MonadIO m) => MonadIO (HandlerM m) where
  liftIO :: IO a -> HandlerM m a
  liftIO = HandlerM . liftIO . liftIO

instance (Monad m) => MonadState Context (HandlerM m) where
  get :: HandlerM m Context
  get = HandlerM $ lift get

  put :: Context -> HandlerM m ()
  put = HandlerM . lift . put


type Handler a = HandlerM IO a

getRequest :: Handler Request
getRequest = fst <$> get

getMethod :: Handler Method
getMethod = requestMethod <$> getRequest

getHeaders :: Handler RequestHeaders
getHeaders = requestHeaders <$> getRequest

getResponse :: Handler Response
getResponse = snd <$> get

setStatus :: Status -> Handler ()
setStatus status = do
  (request, response) <- get
  put (request, mapResponseStatus (const status) response)

setHeader :: Header -> Handler ()
setHeader header = do
  (request, response) <- get
  put (request, mapResponseHeaders ((header:) . filter (header /=)) response)

text :: Text -> Handler ()
text body = do
  setHeader (hContentType, "text/plain")
  request <- getRequest
  response <- getResponse
  let status = responseStatus response
      headers = responseHeaders response
  put (request, responseBuilder status headers $ encodeUtf8Builder body)

execHandler :: Handler () -> Context -> IO Response
execHandler (HandlerM eitherT) context = do
  (either', (_, response)) <- runStateT (runEitherT eitherT) context
  fromLeft (pure response) either' `catch` handleUnexpectedError

application :: Handler () -> Application
application handler request respond = do
  let response = responseBuilder status404 [(hContentType, "text/plain")] "Not found"
      context = (request, response)
  runHandler handler context respond

middleware :: Handler () -> Middleware
middleware handler app request respond =
  app request $ \response -> runHandler handler (request, response) respond

-- -------------------------------- Internal -------------------------------- --

runHandler :: Handler () -> Context -> (Response -> IO ResponseReceived) -> IO ResponseReceived
runHandler handler context respond = do
  respond =<< execHandler handler context

handleUnexpectedError :: SomeException -> IO Response
handleUnexpectedError e = do
  print e
  return $ responseBuilder status500 [] "Internal sever error"
