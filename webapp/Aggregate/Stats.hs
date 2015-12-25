{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Aggregate.Stats
    ( Stats
    , buildStats
    , postView
    , postStats
    ) where

--------------------------------------------------------------------------------
import Control.Concurrent.STM
import GHC.Generics
import Prelude

--------------------------------------------------------------------------------
import           Data.Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S
import           Data.Text
import           Data.Time
import           Database.EventStore

--------------------------------------------------------------------------------
import Aggregate.Post
import Aggregate.Posts hiding (posts)
import Store

--------------------------------------------------------------------------------
data StatEvents = View !UTCTime !String deriving Generic

--------------------------------------------------------------------------------
instance FromJSON StatEvents
instance ToJSON StatEvents

--------------------------------------------------------------------------------
data PostStats =
    PostStats
    { postViews       :: !Int
    , postUniqueViews :: !Int
    , _postIpSet      :: !(S.Set String)
    }

--------------------------------------------------------------------------------
emptyPostStats :: PostStats
emptyPostStats = PostStats 0 0 S.empty

--------------------------------------------------------------------------------
data Stats =
    Stats
    { _conn :: Connection
    , _var  :: TVar (H.HashMap PostId PostStats)
    }

--------------------------------------------------------------------------------
postStream :: PostId -> Text
postStream pid = "stats-" <> (postIdText pid)

--------------------------------------------------------------------------------
buildStats :: Connection -> IO Stats
buildStats _conn = do
    st   <- streamFold _conn "posts" posts H.empty
    _var <- newTVarIO st
    return Stats{..}
  where
    posts m (PostPublished pid) _ = do
        s <- streamFold _conn (postStream pid) stats emptyPostStats
        return $ H.insert pid s m
    posts m (PostUnpublished pid) _ =
        return $ H.delete pid m
    posts m _ _ = return m

    stats m (View _ ip) _ =
        return $ updatePostStats ip m

--------------------------------------------------------------------------------
updatePostStats :: String -> PostStats -> PostStats
updatePostStats ip m
    | S.member ip $ _postIpSet m =
          m { postViews = postViews m + 1 }
    | otherwise =
          let vc  = postViews m + 1
              vuc = postUniqueViews m + 1
              ns  = S.insert ip $ _postIpSet m
              m'  = m { postViews       = vc
                      , postUniqueViews = vuc
                      , _postIpSet      = ns
                      } in
          m'

--------------------------------------------------------------------------------
postView :: Stats -> PostId -> String -> IO ()
postView Stats{..} pid ip = do
    dt <- getCurrentTime
    let view_evt  = View dt ip
        saved_evt = createEvent "user-view" Nothing $ withJson view_evt
    writeView <- sendEvent _conn (postStream pid) anyVersion saved_evt
    atomically $ do
        _ <- waitSTM writeView
        modifyTVar _var (H.adjust (updatePostStats ip) pid)

--------------------------------------------------------------------------------
postStats :: Stats -> PostId -> IO (Maybe PostStats)
postStats Stats{..} pid = fmap (H.lookup pid) $ readTVarIO _var
