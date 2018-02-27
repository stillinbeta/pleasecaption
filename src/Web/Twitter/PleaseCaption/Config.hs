{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.PleaseCaption.Config ( getTWInfo
                                        , Env(..)
                                        ) where

import qualified Data.ByteString.Char8 as S8
import System.Environment (getEnv)
import Web.Twitter.Types (UserId)
import Web.Twitter.Conduit (TWInfo, oauthConsumerKey, oauthConsumerSecret, Credential(..), setCredential, twitterOAuth, def)

import qualified Web.Twitter.PleaseCaption.Client as Client

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

data Env = Env {
  ourClient :: Client.Client,
  ourUserId :: UserId
  }

instance Client.HasClient Env where
  getClient = ourClient
