{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE TemplateHaskell #-}
module Web.Twitter.PleaseCaption.Replies (getReminderText) where

import Control.Monad.IO.Class (liftIO, MonadIO(..))
import Data.Text(Text)
import Data.Random.Extras (choice)
import Data.Random.RVar (runRVar)
import Data.Random.Source.DevRandom (DevRandom(DevURandom))

import Web.Twitter.PleaseCaption.Replies.Assert (assertAllLength)

reminders :: [Text]
-- Warning! Can't be more than 132 characters
reminders = $(assertAllLength 132
  [ "please don't forget to caption your tweets!"
  , "hey, this tweet's images don't all have alt text!"
  , "psst, you missed the alt text in this tweet"
  , "this tweet's images don't have alt text, please add some next time!"
  , "visually-impaired people may have trouble seeing this tweet, please add captions!"
  , "please remember to caption your images, it makes twitter more accessible!"
  , "hey, please add alt text to your images next time"
  , "this tweet is pretty cool, but you know what's even cooler? alt text"
  , "help make twitter more accessible by adding descriptions to your images!"
  -- add more here!
  ])


getReminderText :: (MonadIO m) => m Text
getReminderText = liftIO $ runRVar (choice reminders) DevURandom
