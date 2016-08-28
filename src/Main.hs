{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when, void)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Foldable (forM_)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import System.Environment (getEnv)

import Data.Aeson (FromJSON)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class (liftIO)
import Control.Lens ((^.), (&), (?~))
import Web.Twitter.Conduit hiding (inReplyToStatusId, map)
import Web.Twitter.Conduit.Parameters (inReplyToStatusId, includeExtAltText)
import Web.Twitter.Types
import qualified Web.Twitter.Types.Lens as TL




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


main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  twinfo <- getTWInfo
  userId <- getUserId mgr twinfo
  let state = State { ourUserId = userId, sTwInfo = twinfo, sManager = mgr }
  runResourceT $ do
    src <- stream twinfo mgr userstream
    src C.$$+- CL.mapM_ (liftIO . (printTL state))

getUserId :: Manager -> TWInfo -> IO UserId
getUserId mgr twinfo = do
  user <- call twinfo mgr accountVerifyCredentials
  let User {userId = uid} = user in
    return uid

getStatus :: State -> StatusId -> IO Status
getStatus state sid =
  sCall state $ showId sid & includeExtAltText ?~ True

printTL :: State -> StreamingAPI -> IO ()
printTL state (SStatus s) = handleStatus state s
printTL state (SEvent e) = handleEvent state e
printTL _ s = print s

getExtendedEntities :: Status -> [ExtendedEntity]
getExtendedEntities status =
  fromMaybe [] $ (map entityBody) . exeMedia <$> statusExtendedEntities status

hasImageEntities :: Status -> Bool
hasImageEntities status =
  let entities = getExtendedEntities status in
    not $ null entities && all (=="photo") (map exeType entities)

statusHasAltText :: Status -> Bool
statusHasAltText status =
  let entities = getExtendedEntities status in
    all (isJust . exeExtAltText) entities


getReminderText :: IO T.Text
-- TODO: Will eventually pick one at random
getReminderText = return "please don't forget to caption your tweets!"

replyToStatus :: Status -> T.Text -> APIRequest StatusesUpdate Status
replyToStatus status tweet =
  let reply = T.concat ["@"
                       , status ^. TL.statusUser ^. TL.userScreenName
                       ,  " "
                       , tweet] in
  update reply & inReplyToStatusId ?~ status ^. TL.statusId

handleStatus :: State -> Status -> IO ()
handleStatus state status
  | hasImageEntities status = do
      status' <- getStatus state $ statusId status
      when (not $ statusHasAltText status') $ do
        reminderText <- getReminderText
        let reply = replyToStatus status reminderText
        void $ sCall state reply
  | True = return ()

handleEvent :: State -> Event -> IO ()
handleEvent state Event { evEvent = "follow", evSource = ETUser user}
  -- they followed us
  | userId user /= ourUserId state = do
      let req = friendshipsCreate (UserIdParam $ userId user)
      void $ sCall state req
  -- we followed someone
  | userId user == ourUserId state = putStrLn "we followed someone"
handleEvent _ ev =
  TI.putStrLn $ T.concat ["ignoring event type ", ev ^. TL.evEvent]


printStatus :: Status -> IO ()
printStatus Status { statusText = text,
                        statusUser = User { userScreenName = screenName }} =
  TI.putStrLn $ T.concat [ "@", screenName, ":", text ]
