{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module BitTorrent.Types where

import Control.Lens
import Data.Word

import Network (Socket)
import qualified Data.ByteString as B

type Hash = B.ByteString

data MetainfoFile = MetainfoFile
    { _mtfSize :: Integer
    , _mtfPath :: String
    }

makeFields ''MetainfoFile

data Metainfo = Metainfo
    { _mtAnnounce :: String
    , _mtInfoHash :: B.ByteString
    , _mtName :: B.ByteString
    , _mtPieceLen :: Integer
    , _mtPieces :: [Hash]
    , _mtSize :: Maybe Integer
    , _mtFiles :: Maybe [MetainfoFile]
    }

makeFields ''Metainfo

data Event = Started | Completed | Stopped | Empty
    deriving (Eq)

instance Show Event where
    show Started = "started"
    show Completed = "completed"
    show Stopped = "stopped"
    show Empty = "empty"

data TrackerRequest = TrackerRequest
    { _reqAnnounce :: String
    , _reqInfoHash :: B.ByteString
    , _reqPeerId :: String
    , _reqIp :: String
    , _reqPort :: Int
    , _reqUploaded :: Int
    , _reqDownloaded :: Int
    , _reqLeft :: Int
    , _reqEvent :: Event
    , _reqCompact :: Int
    } deriving (Eq, Show)

makeFields ''TrackerRequest

data Peer = Peer
    { _peerId :: Maybe String
    , _peerIp :: Word32
    , _peerPort :: Word16
    } deriving (Eq, Show)

makeFields ''Peer

data TrackerResponse = TrackerResponse
    { _resInterval :: Int
    , _resPeers :: [Peer]
    } deriving (Eq, Show)

makeFields ''TrackerResponse

data PieceState = PieceDone | PieceStarted | PieceEmpty
