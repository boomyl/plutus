{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC   -Wno-orphans     #-}

module Server
    ( mkHandlers
    )
where

import           API                           (API, RunResult, WSAPI)
import           Control.Monad                 (forever)
import           Control.Monad.Catch           (MonadCatch, MonadMask, bracket, catch)
import           Control.Monad.Except          (MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Logger          (MonadLogger, logInfoN)
import           Data.Aeson                    (ToJSON, encode)
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.Text                     as Text
import           Data.Time.Units               (Microsecond, fromMicroseconds)
import qualified Interpreter
import           Language.Haskell.Interpreter  (InterpreterError (CompilationErrors), InterpreterResult,
                                                SourceCode (SourceCode))
import           Marlowe.Contracts             (escrow)
import           Network.HTTP.Types            (hContentType)
import           Network.WebSockets.Connection (PendingConnection, receiveData, sendTextData)
import           Servant                       (ServantErr, err400, errBody, errHeaders)
import           Servant.API                   ((:<|>) ((:<|>)), (:>), JSON, Post, ReqBody)
import           Servant.Server                (Handler, Server)
import           System.Timeout                (timeout)
import Control.Concurrent.STM.TVar (TVar, modifyTVar)
import Control.Concurrent.STM (atomically)
import WebSocket (Registry, newRegistry, initializeConnection, runWithConnection, insertIntoRegistry, deleteFromRegistry)

acceptSourceCode :: SourceCode -> Handler (Either InterpreterError (InterpreterResult RunResult))
acceptSourceCode sourceCode = do
    let maxInterpretationTime :: Microsecond = fromMicroseconds (10 * 1000 * 1000)
    r <-
        liftIO
        $ runExceptT
        $ Interpreter.runHaskell maxInterpretationTime sourceCode
    case r of
        Right vs                        -> pure $ Right vs
        Left (CompilationErrors errors) -> pure . Left $ CompilationErrors errors
        Left  e                         -> throwError $ err400 { errBody = BSL.pack . show $ e }

checkHealth :: Handler ()
checkHealth = do
    res <- acceptSourceCode . SourceCode . Text.pack . BS.unpack $ escrow
    case res of
        Left e  -> throwError $ err400 {errBody = BSL.pack . show $ e}
        Right _ -> pure ()

-- TODO: Here we're going to recieve JSON and pass it through to the
--       AWS API Gateway to be processed. We're also going to receive
--       messages from the AWS Lambda (I think we need to provide another
--       endpoint for the Lambda to reach) which we will push down this socket
handleWS :: TVar Registry -> PendingConnection -> Handler ()
handleWS registry pending = liftIO $ do
    (uuid, connection) <- initializeConnection pending
    atomically . modifyTVar registry $ insertIntoRegistry uuid connection
    runWithConnection connection f
    atomically . modifyTVar registry $ deleteFromRegistry uuid
    putStrLn "closed connection"
    where
        f :: Text.Text -> IO ()
        f = putStrLn . Text.unpack


{-# ANN mkHandlers
          ("HLint: ignore Avoid restricted function" :: String)
        #-}

mkHandlers :: (MonadLogger m, MonadIO m) => m (Server (API :<|> WSAPI))
mkHandlers = do
    logInfoN "Interpreter ready"
    registry <- liftIO $ atomically newRegistry
    pure $ (acceptSourceCode :<|> checkHealth) :<|> (handleWS registry)
