module Main where

import Control.Monad.Reader (runReaderT)
import qualified Data.Set as Set
import Web.Twitter.Conduit (newManager, tlsManagerSettings)
import Control.Monad.Base (MonadBase, liftBase)

import Web.Twitter.PleaseCaption.Config (getTWInfo)
import qualified Web.Twitter.PleaseCaption.Client as Client

main :: IO ()
main = do
  putStrLn "pleasecaption is pruning follow list"
  mgr <- newManager tlsManagerSettings
  twinfo <- getTWInfo
  let client = Client.Client { Client.twInfo = twinfo, Client.manager = mgr }
  flip runReaderT client $ do
    uid <- Client.getUserId
    following <- Set.fromList <$> Client.getFollowees uid
    followers <- Set.fromList <$> Client.getFollowers uid
    let toUnfollow = Set.toList $ following `Set.difference` followers
    liftBase $ putStrLn $ "unfollowing " ++ show (length toUnfollow) ++ " users"
    mapM_ Client.unfollowUser toUnfollow
  putStrLn "all done"
