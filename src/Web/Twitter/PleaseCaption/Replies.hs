{-# LANGUAGE OverloadedStrings#-}
module Web.Twitter.PleaseCaption.Replies (getReminderText) where

import Data.Text(Text)
import Data.Random.Extras (choice)
import Data.Random.RVar (runRVar)
import Data.Random.Source.DevRandom (DevRandom(DevURandom))

reminders :: [Text]
reminders =
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
  ]

getReminderText :: IO Text
-- TODO: Will eventually pick one at random
getReminderText = runRVar (choice reminders) DevURandom
