{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as S8
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.List as ConduitList
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Control.Monad (when, unless, void, forever)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (runReaderT, ReaderT, asks)
import Control.Monad.IO.Class (liftIO)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.IO.Error (catchIOError)
import System.Environment (getEnv)

import Web.Twitter.Conduit hiding (inReplyToStatusId, map, replies)
import Web.Twitter.Types (StreamingAPI(..), Event(..), EventTarget(..),
                          UserId, Status(..), User(..))

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
  putStrLn "pleasecaption: starting up!"
  mgr <- newManager tlsManagerSettings
  twinfo <- getTWInfo
  let client = Client.Client { Client.twInfo = twinfo, Client.manager = mgr }
  uid <- runReaderT Client.getUserId client
  let env = Env { ourClient = client, ourUserId = uid}
  forever $ catchIOError (runReaderT runStream env) logError

logError :: IOError -> IO ()
logError err = putStr "encountered error: " >> print err

runStream :: ReaderT Env IO ()
runStream = runResourceT $ do
    src <- Client.startStream
    let sink = ConduitList.mapM_ $ lift . handleTL
    src Conduit.$$+- sink

handleTL :: StreamingAPI -> ReaderT Env IO ()
handleTL (SStatus s) = handleStatus s
handleTL (SEvent e) = handleEvent e
handleTL s = return ()

handleStatus :: Status -> ReaderT Env IO ()
handleStatus status =
  when (Status.hasPhotoEntities status) $ do
      status' <- Client.askStatus $ statusId status
      unless (Status.hasAltText status') $ do
        reminderText <- liftIO Replies.getReminderText
        void $ Client.replyToStatus status reminderText


handleEvent :: Event -> ReaderT Env IO ()
handleEvent Event { evEvent = "follow", evSource = ETUser user} = do
  ourID <- asks ourUserId
  -- Someone followed us
  when (userId user /= ourID) $ do
    liftIO $ TextIO.putStrLn $ Text.concat [ "following user "
                                           , userScreenName user]
    void $ Client.followUser user
handleEvent ev =
  liftIO $ TextIO.putStrLn $ Text.concat ["ignoring event type ", evEvent ev]
