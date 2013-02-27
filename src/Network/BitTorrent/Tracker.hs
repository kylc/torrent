module Network.BitTorrent.Tracker where

import Network.HTTP

import Network.BitTorrent.Bencode
import Network.BitTorrent.Metainfo

data TrackerRequest = TrackerRequest {
    reqAnnounce :: String
  , reqInfoHash :: String
  , reqPeerId :: String
  , reqIp :: String
  , reqPort :: Int
  , reqUploaded :: Int
  , reqDownloaded :: Int
  , reqLeft :: Int
  , reqEvent :: Event
  } deriving (Eq, Show)

data TrackerResponse = TrackerResponse {
    resInterval :: Int
  , resPeers :: [Peer]
  } deriving (Eq, Show)

data Peer = Peer {
    peerId :: String
  , peerIp :: String
  , peerPort :: Int
  } deriving (Eq, Show)

data Event = Started | Completed | Stopped | Empty
    deriving (Eq)

instance Show Event where
    show Started = "started"
    show Completed = "completed"
    show Stopped = "stopped"
    show Empty = "empty"

request :: TrackerRequest -> IO TrackerResponse
request req = do
    rsp <- simpleHTTP $ getRequest url
    return TrackerResponse { resInterval = 10
                           , resPeers = []
                           }
  where
    url = reqAnnounce req ++ "?" ++ requestParams req


requestParams :: TrackerRequest -> String
requestParams req = urlEncodeVars
    [ ("info_hash", reqInfoHash req)
    , ("peer_id", reqPeerId req)
    , ("ip", reqIp req)
    , ("port", show $ reqPort req)
    , ("uploaded", show $ reqUploaded req)
    , ("downloaded", show $ reqDownloaded req)
    , ("left", show $ reqLeft req)
    , ("event", show $ reqEvent req)
    ]
