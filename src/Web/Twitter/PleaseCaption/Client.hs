{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Web.Twitter.PleaseCaption.Client ( Client(..)
                                        , HasClient(..)
                                        , askStatus
                                        , replyToStatus
                                        , followUser
                                        , getUserId
                                        , startStream) where

import Control.Lens ((&), (?~))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Trans.Class (lift, MonadTrans(..))
import Control.Monad.Reader(MonadReader, asks)
import qualified Data.Aeson as Aeson
import qualified Data.Conduit as Conduit
import qualified Data.Text as Text
import Web.Twitter.Conduit (TWInfo, Manager, APIRequest, call,
                            stream, showId, update, friendshipsCreate,
                            userstream, accountVerifyCredentials)
import Web.Twitter.Conduit.Parameters (includeExtAltText, inReplyToStatusId,
                                       UserParam(UserIdParam), replies)
import Web.Twitter.Types (StatusId, Status(statusId, statusUser),
                          User(userScreenName, userId, userId), StreamingAPI)

data Client = Client { twInfo :: TWInfo,
                       manager :: Manager
                     }

class HasClient a where
  getClient :: a -> Client

instance HasClient Client where
  getClient = id

sCall :: (HasClient c, MonadReader c m, MonadIO m, Aeson.FromJSON responseType)
      => APIRequest apiName responseType
      -> m responseType
sCall request = do
  Client twinfo mgr <- asks getClient
  liftIO $ call twinfo mgr request

getUserId :: (HasClient c, MonadReader c m, MonadIO m) => m StatusId
getUserId = do
  user <- sCall accountVerifyCredentials
  return $ userId user


askStatus :: (HasClient c, MonadReader c m, MonadIO m) => StatusId -> m Status
askStatus sid = sCall $ showId sid & includeExtAltText ?~ True

makeReplyBody :: Status -> Text.Text -> Text.Text
makeReplyBody status tweet = Text.concat ["@"
                                      , (userScreenName . statusUser) status
                                      ," "
                                      , tweet
                                      ]

replyToStatus :: (HasClient c, MonadReader c m, MonadIO m)
              => Status -> Text.Text -> m Status
replyToStatus status tweet =
  let reply = update (makeReplyBody status tweet)
               & inReplyToStatusId ?~ statusId status in
   sCall reply

followUser ::  (HasClient c, MonadReader c m, MonadIO m) => User -> m User
followUser user =
  sCall $ friendshipsCreate (UserIdParam $ userId user)


-- Should be wrapped in a reader, but I don't know how ;_;
startStream :: (HasClient c, MonadReader c m, MonadResource m)
          =>  m (Conduit.ResumableSource m StreamingAPI)
startStream = do
  Client { twInfo = twinfo, manager = mgr } <- asks getClient
  stream twinfo mgr $ userstream & replies ?~ "all"
