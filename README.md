# Pleasecaption

please caption your tweets! pleasecaption is a twitter bot that
politely reminds you that you forgot to add alt text to an image you posted.

## General UI

* To get pleasecaption to watch your tweets, follow @pleasecaption on twitter
  ** if your account is protected, it will ask to follow you back
* If your twitter account is private, send @pleasecaption a DM with anything in it
* To get pleasecaption to stop pestering you, block or softblock it. It won't
  attempt to refollow you unless you unfollow and refollow it.


## Running

pleasecaption is a stack project, so `stack install` should set everything up.

To run, `pleasecaption` needs the following env variables available:

* `OAUTH_CONSUMER_KEY`
* `OAUTH_CONSUMER_SECRET`
* `OAUTH_ACCESS_TOKEN`
* `OAUTH_ACCESS_SECRET`

You can get the first two by registering an app on twitter,
and the second two from your user profile.
