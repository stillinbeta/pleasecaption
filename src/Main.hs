{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when, void, forever)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.IO.Error (catchIOError)
import System.Environment (getEnv)

import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (runReaderT, ReaderT(..), asks, ask)
import Control.Monad.IO.Class (liftIO)
import Control.Lens ((^.))
import Web.Twitter.Conduit hiding (inReplyToStatusId, map, replies)
import Web.Twitter.Types (StreamingAPI(..), Event(..), EventTarget(..),
                          UserId, Status(..), User(..))
import qualified Web.Twitter.Types.Lens as TL

import qualified Web.Twitter.PleaseCaption.Replies as Replies
import qualified Web.Twitter.PleaseCaption.Status as Status
import qualified Web.Twitter.PleaseCaption.Client as Client


data Env = Env {
  ourClient :: Client.Client,
  ourUserId :: UserId
  }

instance Client.HasClient Env where
  getClient = ourClient

getTWInfo :: IO TWInfo
getTWInfo = do
    consumerKey <- getEnv' "OAUTH_CONSUMER_KEY"
    consumerSecret <- getEnv' "OAUTH_CONSUMER_SECRET"
    accessToken <- getEnv' "OAUTH_ACCESS_TOKEN"
    accessSecret <- getEnv' "OAUTH_ACCESS_SECRET"
    let oauth = twitterOAuth
            { oauthConsumerKey = consumerKey
            , oauthConsumerSecret = consumerSecret
            }
        cred = Credential
            [ ("oauth_token", accessToken)
            , ("oauth_token_secret", accessSecret)
            ]
    return $ setCredential oauth cred def
  where
    getEnv' = (S8.pack <$>) . getEnv

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "pleasecaption: starting up"
  mgr <- newManager tlsManagerSettings
  twinfo <- getTWInfo
  let client = Client.Client { Client.twInfo = twinfo, Client.manager = mgr }
  uid <- runReaderT Client.getUserId client
  let env = Env { ourClient = client, ourUserId = uid}
  forever $ catchIOError (runReaderT runStream env) logError

logError :: IOError -> IO ()
logError = print

runStream :: ReaderT Env IO ()
runStream = runResourceT $ do
    src <- Client.startStream
    env <- ask
    src C.$$+- CL.mapM_ $ (\s -> liftIO (runReaderT (handleTL s) env))

handleTL :: StreamingAPI -> ReaderT Env IO ()
handleTL (SStatus s) = handleStatus s
handleTL (SEvent e) = handleEvent e
handleTL s = liftIO $ print s

getUserId :: Manager -> TWInfo -> IO UserId
getUserId mgr twinfo = do
  user <- call twinfo mgr accountVerifyCredentials
  let User {userId = uid} = user in
    return uid

handleStatus :: Status -> ReaderT Env IO ()
handleStatus status
  | Status.hasPhotoEntities status = do
      status' <- Client.askStatus $ statusId status
      when (not $ Status.hasAltText status') $ do
        reminderText <- liftIO Replies.getReminderText
        void $ Client.replyToStatus status reminderText
  | True = liftIO $ TI.putStrLn (Status.asText status)


handleEvent :: Event -> ReaderT Env IO ()
handleEvent Event { evEvent = "follow", evSource = ETUser user} = do
  ourID <- asks ourUserId
  -- Someone followed us
  when (userId user /= ourID) $ do
    liftIO $ TI.putStrLn $ T.concat [ "following user ", user ^. TL.userScreenName ]
    -- void $ followUser user
handleEvent ev =
  liftIO $ TI.putStrLn $ T.concat ["ignoring event type ", ev ^. TL.evEvent]
