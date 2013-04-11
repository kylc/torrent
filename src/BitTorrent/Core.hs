module BitTorrent.Core
    ( run
    ) where

import Control.Concurrent
import Control.Concurrent.Delay
import Control.Monad
import Data.Maybe

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP

import BitTorrent.Bencode
import BitTorrent.Metainfo
import BitTorrent.PeerManager
import BitTorrent.PieceManager
import BitTorrent.Tracker
import BitTorrent.Types

run :: String -> IO ()
run f = do
    -- TODO: What if this fails?
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable

    -- Read and decode the .torrent file
    b <- parseBencodeFile f

    -- Request tracker information
    -- TODO: Send some real values here
    -- TODO: Extract this into a process
    let metainfo = fromJust $ readMetainfo b
        req = TrackerRequest { reqAnnounce = mtAnnounce metainfo
                             , reqInfoHash = mtInfoHash metainfo
                             , reqPeerId = "abcdefghijklmnopqrst"
                             , reqIp = "0.0.0.0"
                             , reqPort = 5555
                             , reqUploaded = 0
                             , reqDownloaded = 0
                             , reqLeft = 0
                             , reqEvent = Started
                             , reqCompact = 1
                             }
    resp <- request req

    -- Connect to peers
    case resp of
        Right resp ->
            forkProcess node $ void $ do
                spawnLocal $ runPieceMgr metainfo
                spawnLocal $ runPeerMgr metainfo $ resPeers resp
        Left e -> fail $ "Failed to parse bencode file: " ++ e

    -- Download!
    forever $ delaySeconds 1

    return ()
