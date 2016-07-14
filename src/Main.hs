{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Twitter.Conduit
import Web.Twitter.Types.Lens

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Lens
import System.Environment
import qualified Data.ByteString.Char8 as S8

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as TI

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
  runResourceT $ do
    src <- stream twinfo mgr userstream
    src C.$$+- CL.mapM_ (liftIO . printTL)

printTL :: StreamingAPI -> IO ()
printTL (SStatus s) = TI.putStrLn . showStatus $ s
printTL s = print s

showStatus :: AsStatus s => s -> T.Text
showStatus s = T.concat [ s ^. user . userScreenName
                        , ":"
                        , s ^. text
                        ]
