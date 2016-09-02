{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when, void)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.Environment (getEnv)

import Data.Aeson (FromJSON)
import Control.Monad.Trans.Resource (runResourceT, MonadResource)
import Control.Monad.Reader (runReaderT, ReaderT(..), asks)
import Control.Monad.IO.Class (liftIO)
import Control.Lens ((^.), (&), (?~))
import Web.Twitter.Conduit hiding (inReplyToStatusId, map, replies)
import Web.Twitter.Conduit.Parameters (inReplyToStatusId, includeExtAltText, replies)
import Web.Twitter.Types
import qualified Web.Twitter.Types.Lens as TL

import Web.Twitter.PleaseCaption.Replies (getReminderText)


data State = State {
  sTwInfo   :: TWInfo,
  sManager  :: Manager,
  ourUserId :: UserId
  }

sCall :: FromJSON responseType
      => APIRequest apiName responseType
      -> ReaderT State IO responseType
sCall request = do
  twinfo <- asks sTwInfo
  manager <- asks sManager
  liftIO $ call twinfo manager request

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


askstream :: (MonadResource m0)
          => State -> m0 (C.ResumableSource m0 StreamingAPI)
askstream State {sTwInfo = twinfo, sManager = mgr} =
  stream twinfo mgr $ userstream & replies ?~ "all"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "pleasecaption: starting up"
  mgr <- newManager tlsManagerSettings
  twinfo <- getTWInfo
  uid <- getUserId mgr twinfo
  let state = State { ourUserId = uid, sTwInfo = twinfo, sManager = mgr }
  runResourceT $ do
    src <- stream twinfo mgr $ userstream & replies ?~ "all"
    src C.$$+- CL.mapM_ (liftIO . (\s -> runReaderT (printTL s) state))

printTL :: StreamingAPI -> ReaderT State IO ()
printTL (SStatus s) = handleStatus s
printTL (SEvent e) = handleEvent e
printTL s = liftIO $ print s

getUserId :: Manager -> TWInfo -> IO UserId
getUserId mgr twinfo = do
  user <- call twinfo mgr accountVerifyCredentials
  let User {userId = uid} = user in
    return uid

askstatus :: StatusId -> ReaderT State IO Status
askstatus sid =
  sCall $ showId sid & includeExtAltText ?~ True

getExtendedEntities :: Status -> [ExtendedEntity]
getExtendedEntities status =
  fromMaybe [] $ (map entityBody) . exeMedia <$> statusExtendedEntities status

statusHasPhotoEntities :: Status -> Bool
statusHasPhotoEntities status =
  let entities = getExtendedEntities status in
    not $ null entities && all (=="photo") (map exeType entities)

statusHasAltText :: Status -> Bool
statusHasAltText status =
  let entities = getExtendedEntities status in
    all (isJust . exeExtAltText) entities


replyToStatus :: Status -> T.Text -> APIRequest StatusesUpdate Status
replyToStatus status tweet =
  let reply = T.concat ["@"
                       , status ^. TL.statusUser ^. TL.userScreenName
                       ,  " "
                       , tweet] in
  update reply & inReplyToStatusId ?~ status ^. TL.statusId

handleStatus :: Status -> ReaderT State IO ()
handleStatus status
  | statusHasPhotoEntities status = do
      status' <- askstatus $ statusId status
      when (not $ statusHasAltText status') $ do
        reminderText <- liftIO getReminderText
        let reply = replyToStatus status reminderText
        void $ sCall reply
  | True = liftIO $ printStatus status

handleEvent :: Event -> ReaderT State IO ()
handleEvent Event { evEvent = "follow", evSource = ETUser user} = do
  ourID <- asks ourUserId
  -- Someone followed us
  when (userId user /= ourID) $ do
    let req = friendshipsCreate (UserIdParam $ userId user)
    liftIO $ TI.putStrLn $ T.concat [ "following user ", user ^. TL.userScreenName ]
    void $ sCall req
handleEvent ev =
  liftIO $ TI.putStrLn $ T.concat ["ignoring event type ", ev ^. TL.evEvent]


printStatus :: Status -> IO ()
printStatus Status { statusText = text,
                     statusUser = User { userScreenName = screenName }} =
  TI.putStrLn $ T.concat [ "@", screenName, ":", text ]
