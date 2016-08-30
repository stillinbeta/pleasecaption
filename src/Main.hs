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

sCall :: FromJSON responseType => State -> APIRequest apiName responseType -> IO responseType
sCall (State { sTwInfo = twinfo, sManager = manager}) = call twinfo manager

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


getStream :: (MonadResource m0)
          => State -> m0 (C.ResumableSource m0 StreamingAPI)
getStream State {sTwInfo = twinfo, sManager = mgr} =
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
    src C.$$+- CL.mapM_ (liftIO . (printTL state))

printTL :: State -> StreamingAPI -> IO ()
printTL state (SStatus s) = handleStatus state s
printTL state (SEvent e) = handleEvent state e
printTL _ s = print s

getUserId :: Manager -> TWInfo -> IO UserId
getUserId mgr twinfo = do
  user <- call twinfo mgr accountVerifyCredentials
  let User {userId = uid} = user in
    return uid

getStatus :: State -> StatusId -> IO Status
getStatus state sid =
  sCall state $ showId sid & includeExtAltText ?~ True

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

handleStatus :: State -> Status -> IO ()
handleStatus state status
  | statusHasPhotoEntities status = do
      status' <- getStatus state $ statusId status
      when (not $ statusHasAltText status') $ do
        reminderText <- getReminderText
        let reply = replyToStatus status reminderText
        void $ sCall state reply
  | True = printStatus status

handleEvent :: State -> Event -> IO ()
handleEvent state Event { evEvent = "follow", evSource = ETUser user}
  -- they followed us
  | userId user /= ourUserId state = do
      let req = friendshipsCreate (UserIdParam $ userId user)
      TI.putStrLn $ T.concat [ "following user ", user ^. TL.userScreenName ]
      void $ sCall state req
  -- we followed someone
  | userId user == ourUserId state = return ()
handleEvent _ ev =
  TI.putStrLn $ T.concat ["ignoring event type ", ev ^. TL.evEvent]


printStatus :: Status -> IO ()
printStatus Status { statusText = text,
                     statusUser = User { userScreenName = screenName }} =
  TI.putStrLn $ T.concat [ "@", screenName, ":", text ]
