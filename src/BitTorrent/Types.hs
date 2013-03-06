module BitTorrent.Types where

import qualified Data.ByteString as B
import Data.Word

type Hash = B.ByteString

data Metainfo = Metainfo
    { mtAnnounce :: String
    , mtInfoHash :: B.ByteString
    , mtName :: B.ByteString
    , mtPieceLen :: Integer
    , mtPieces :: [Hash]
    , mtLength :: Maybe Integer
    , mtFiles :: Maybe [MetainfoFile]
    }

data MetainfoFile = MetainfoFile
    { mtfLength :: Integer
    , mtfPath :: String
    }

data TrackerRequest = TrackerRequest
    { reqAnnounce :: String
    , reqInfoHash :: B.ByteString
    , reqPeerId :: String
    , reqIp :: String
    , reqPort :: Int
    , reqUploaded :: Int
    , reqDownloaded :: Int
    , reqLeft :: Int
    , reqEvent :: Event
    , reqCompact :: Int
    } deriving (Eq, Show)

data TrackerResponse = TrackerResponse
    { resInterval :: Int
    , resPeers :: [Peer]
    } deriving (Eq, Show)

data Event = Started | Completed | Stopped | Empty
    deriving (Eq)

instance Show Event where
    show Started = "started"
    show Completed = "completed"
    show Stopped = "stopped"
    show Empty = "empty"

data Peer = Peer
    { peerId :: String
    , peerIp :: Word32
    , peerPort :: Word16
    } deriving (Eq, Show)

-- TODO: Fill these in
data Message = Choke
             | Unchoke
             | Interested
             | NotInterested
             | Have
             | BitField
             | Request
             | Piece
             | Cancel
