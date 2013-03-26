{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

module BitTorrent.Types where

import Data.Typeable (Typeable)
import Data.Word
import GHC.Generics (Generic)

import Data.Binary (Binary)
import qualified Data.ByteString as B
import Network (Socket)

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
    { peerId :: Maybe String
    , peerIp :: Word32
    , peerPort :: Word16
    } deriving (Eq, Show)

data PeerState = PeerState
    { peerSocket :: Maybe Socket
    , peerChoked :: Bool
    , peerInterested :: Bool
    , peerAmChoking :: Bool
    , peerAmInterested :: Bool
    } deriving (Eq, Show)

defaultPeerState :: Int -> PeerState
defaultPeerState pieceCount = PeerState
    { peerSocket = Nothing
    , peerChoked = False
    , peerInterested = False
    , peerAmChoking = False
    , peerAmInterested = False
    }

data PieceState = PieceDone | PieceStarted | PieceEmpty

type ProtocolName = B.ByteString
type ProtocolExt = B.ByteString
type PeerID = B.ByteString

data PeerMessage = KeepAlive
                 | Handshake ProtocolName ProtocolExt Hash PeerID
                 | Choke
                 | Unchoke
                 | Interested
                 | NotInterested
                 | Have Int
                 | Bitfield B.ByteString
                 | Request Int Int Int
                 | Piece Int Int B.ByteString
                 | Cancel Int Int Int
                 | Port Int
    deriving (Eq, Show, Generic, Typeable)

instance Binary PeerMessage

data ProcMessage = PeerRecv PeerMessage
                 | PeerHas Int
                 | PeerFetch Int
    deriving (Eq, Show, Generic, Typeable)

instance Binary ProcMessage
