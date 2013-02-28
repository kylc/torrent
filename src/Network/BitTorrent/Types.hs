module Network.BitTorrent.Types where

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
  , reqCompact :: Int
  } deriving (Eq, Show)

data TrackerResponse = TrackerResponse {
    resInterval :: Int
  , resPeers :: [Peer]
  } deriving (Eq, Show)

data Event = Started | Completed | Stopped | Empty
    deriving (Eq)

instance Show Event where
    show Started = "started"
    show Completed = "completed"
    show Stopped = "stopped"
    show Empty = "empty"

data Peer = Peer {
    peerId :: String
  , peerIp :: String
  , peerPort :: Int
  } deriving (Eq, Show)

