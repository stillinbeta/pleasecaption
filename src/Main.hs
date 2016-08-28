{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as S8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Foldable (forM_)
import Data.Maybe (catMaybes, fromMaybe)
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
  ourUserId :: UserId,
  sTwInfo :: TWInfo,
  sManager :: Manager
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
  uid <- getUserId mgr twinfo
  let state = State { ourUserId = uid, sTwInfo = twinfo, sManager = mgr }
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
printTL _ s = print s

getExtendedEntities :: Status -> [ExtendedEntity]
getExtendedEntities status =
  fromMaybe [] $ (map entityBody) . exeMedia <$> statusExtendedEntities status

hasImageEntities :: Status -> Bool
hasImageEntities status =
  let entities = getExtendedEntities status in
    not $ null entities && all (=="photo") (map exeType entities)

handleStatus :: State -> Status -> IO ()
-- handleStatus
--   State { ourUserId = uid,
--           sManager = mgr,
--           sTwInfo = twinfo }
--   Status { statusId = sid,
--            statusInReplyToUserId = (Just ruid),
--            statusUser = user,
--            statusText = text}
--   | uid == ruid = do
--       TI.putStrLn $ "got mentioned!" `T.append` text
--       let reply = T.concat ["@", user ^. TL.screen_name,  " hey yourself!"]
--       res <- call twinfo mgr $ update reply & inReplyToStatusId ?~ sid
--       print res

handleStatus state status
  | hasImageEntities status = do
      status' <- getStatus state $ statusId status
      let entities = getExtendedEntities status'
      let altTexts = catMaybes $ map exeExtAltText entities
      putStrLn $ "alt text received: " ++ show entities
      forM_ altTexts $ putStrLn . ("alt text: " ++)
  | True = printStatus status

printStatus :: Status -> IO ()
printStatus Status { statusText = text,
                        statusUser = User { userScreenName = screenName }} =
  TI.putStrLn $ T.concat [ "@", screenName, ":", text ]
