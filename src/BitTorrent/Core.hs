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
import BitTorrent.Tracker
import BitTorrent.Types

run :: String -> IO ()
run f = do
    Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
    node <- newLocalNode t initRemoteTable

    -- Read and decode the .torrent file
    b <- parseBencodeFile f

    -- Request tracker information
    let metainfo = fromJust $ readMetainfo b
        req = TrackerRequest { reqAnnounce = mtAnnounce metainfo
                             , reqInfoHash = mtInfoHash metainfo
                             , reqPeerId = "abcdefghijklmnopqrst" -- TODO
                             , reqIp = "0.0.0.0" -- TODO
                             , reqPort = 5555 -- TODO
                             , reqUploaded = 0
                             , reqDownloaded = 0
                             , reqLeft = 0
                             , reqEvent = Started
                             , reqCompact = 1
                             }
    resp <- request req

    -- Connect to peers
    case resp of
        Right resp -> forkProcess node $
            void $ spawnLocal . runPeerMgr metainfo $ resPeers resp
        Left e -> fail $ "Failed to parse bencode file: " ++ e

    -- Download!
    forever $ delaySeconds 1

    return ()
