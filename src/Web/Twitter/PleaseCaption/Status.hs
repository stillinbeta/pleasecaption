{-# LANGUAGE OverloadedStrings #-}

module Web.Twitter.PleaseCaption.Status ( getExtendedEntities
                                        , hasPhotoEntities
                                        , hasAltText
                                        , asText
                                        ) where

import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as Text

import Web.Twitter.Types (Status(..), ExtendedEntities(..),
                          ExtendedEntity(..), Entity(..), User(..))

getExtendedEntities :: Status -> [ExtendedEntity]
getExtendedEntities status =
  fromMaybe [] $ (map entityBody) . exeMedia <$> statusExtendedEntities status

hasPhotoEntities :: Status -> Bool
hasPhotoEntities status =
  let entities = getExtendedEntities status in
    not $ null entities && all (=="photo") (map exeType entities)

hasAltText :: Status -> Bool
hasAltText status =
  let entities = getExtendedEntities status in
    all (isJust . exeExtAltText) entities

asText :: Status -> Text.Text
asText Status { statusText = text,
                      statusUser = User { userScreenName = screenName }} =
  Text.concat [ "@", screenName, ":", text ]
