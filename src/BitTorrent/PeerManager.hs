module BitTorrent.PeerManager
    ( runPeerMgr
    ) where

import Control.Concurrent
import Control.Concurrent.Delay
import Control.Monad

import Control.Distributed.Process

import BitTorrent.Peer
import BitTorrent.Types

runPeerMgr :: Metainfo -> [Peer] -> Process ()
runPeerMgr m ps = do
    -- Open connections
    children <- forM ps $ spawnLocal . runPeer m

    -- Wait a bit for all incoming bitfields and haves to arrive
    -- TODO: How long should we wait?  Or should we have a smarter approach?
    liftIO $ delaySeconds 3

    -- TODO: Watch peer processes, restarting if necessary
