{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.Twitter.PleaseCaption.Client ( Client(..)
                                        , HasClient(..)
                                        , askStatus
                                        , replyToStatus
                                        , followUser
                                        , unfollowUser
                                        , getUserId
                                        , getFollowees
                                        , getFollowers
                                        , startStream) where

import Control.Lens ((&), (?~))
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Reader(MonadReader, asks)
import Control.Monad.Base (MonadBase, liftBase)
import qualified Data.Aeson as Aeson
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as Text
import Web.Twitter.Conduit (TWInfo, Manager, APIRequest, call, stream, showId,
                            update, friendshipsCreate, friendshipsDestroy,
                            userstream, accountVerifyCredentials, friendsIds,
                            followersIds, sourceWithCursor )
import Web.Twitter.Conduit.Parameters (includeExtAltText, inReplyToStatusId,
                                       UserParam(UserIdParam), replies, HasCursorParam)
import Web.Twitter.Conduit.Cursor (CursorKey, WithCursor)
import Web.Twitter.Types (StatusId, Status(statusId, statusUser),
                          User(userScreenName, userId, userId), StreamingAPI, UserId)

data Client = Client { twInfo :: TWInfo,
                       manager :: Manager
                     }

class HasClient a where
  getClient :: a -> Client

instance HasClient Client where
  getClient = id

sCall :: (HasClient c, MonadReader c m, MonadBase IO m, Aeson.FromJSON responseType)
      => APIRequest apiName responseType
      -> m responseType
sCall request = do
  Client twinfo mgr <- asks getClient
  liftBase $ call twinfo mgr request

sCallCursor :: (MonadBase IO m,
                Aeson.FromJSON responseType,
                CursorKey ck,
                HasCursorParam (APIRequest apiName (WithCursor ck responseType)),
                HasClient c, MonadReader c m
               )
  => APIRequest apiName (WithCursor ck responseType)
  -> m [responseType]
sCallCursor req = do
  Client twinfo mgr <- asks getClient
  sourceWithCursor twinfo mgr req Conduit.$$ CL.consume

getUserId :: (HasClient c, MonadReader c m, MonadBase IO m) => m StatusId
getUserId = do
  user <- sCall accountVerifyCredentials
  return $ userId user


askStatus :: (HasClient c, MonadReader c m, MonadBase IO m) => StatusId -> m Status
askStatus sid = sCall $ showId sid & includeExtAltText ?~ True

makeReplyBody :: Status -> Text.Text -> Text.Text
makeReplyBody status tweet =
  Text.concat ["@"
              , (userScreenName . statusUser) status
              ," "
              , tweet
              ]

replyToStatus :: (HasClient c, MonadReader c m, MonadBase IO m)
              => Status -> Text.Text -> m Status
replyToStatus status tweet =
  let reply = update (makeReplyBody status tweet)
               & inReplyToStatusId ?~ statusId status in
   sCall reply

followUser ::  (HasClient c, MonadReader c m, MonadBase IO m) => UserId -> m User
followUser uid = sCall $ friendshipsCreate (UserIdParam uid)

unfollowUser ::  (HasClient c, MonadReader c m, MonadBase IO m) => UserId -> m User
unfollowUser uid = sCall $ friendshipsDestroy (UserIdParam uid)

getFollowers :: (HasClient c, MonadReader c m, MonadBase IO m) => UserId -> m [UserId]
getFollowers uid = sCallCursor $ followersIds (UserIdParam uid)

getFollowees :: (HasClient c, MonadReader c m, MonadBase IO m) => UserId -> m [UserId]
getFollowees uid = sCallCursor $ friendsIds (UserIdParam uid)


-- Should be wrapped in a reader, but I don't know how ;_;
startStream :: (HasClient c, MonadReader c m, MonadResource m)
          =>  m (Conduit.ResumableSource m StreamingAPI)
startStream = do
  Client { twInfo = twinfo, manager = mgr } <- asks getClient
  stream twinfo mgr $ userstream & replies ?~ "all"
