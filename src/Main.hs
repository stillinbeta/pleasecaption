{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Twitter.Conduit hiding (inReplyToStatusId)
import Web.Twitter.Conduit.Parameters (inReplyToStatusId)
import Web.Twitter.Types
import qualified Web.Twitter.Types.Lens as TL

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Lens ((^.), (&), (?~))
import System.Environment (getEnv)

import qualified Data.ByteString.Char8 as S8
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as TI

data State = State {
  ourUserId :: UserId,
  sTwInfo :: TWInfo,
  sManager :: Manager
  }

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


printTL :: State -> StreamingAPI -> IO ()
printTL state (SStatus s) = handleStatus state s
printTL _ s = print s

handleStatus :: State -> Status -> IO ()
handleStatus
  State { ourUserId = uid,
          sManager = mgr,
          sTwInfo = twinfo }
  Status { statusId = sid,
           statusInReplyToUserId = (Just ruid),
           statusUser = user,
           statusText = text}
  | uid == ruid = do
      TI.putStrLn $ "got mentioned!" `T.append` text
      let reply = T.concat ["@", user ^. TL.screen_name,  " hey yourself!"]
      res <- call twinfo mgr $ update reply & inReplyToStatusId ?~ sid
      print res


handleStatus _ Status { statusText = text,
                        statusUser = User { userScreenName = screenName }} =
  TI.putStrLn $ T.concat [ "@", screenName, ":", text ]
