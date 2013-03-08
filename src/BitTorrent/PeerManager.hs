module BitTorrent.PeerManager
    ( runPeerMgr
    ) where

import Control.Concurrent
import Control.Monad.State

import BitTorrent.Peer
import BitTorrent.Types

runPeerMgr :: Metainfo -> [Peer] -> IO ()
runPeerMgr m ps = do
    -- Open connections
    forM_ ps $ \p -> forkIO $
        void $ execStateT (runPeer m p) defaultPeerState

    -- Loop until download finishes, computing interests
    forever $ do
        putStrLn "Tick"
        threadDelay $ 1000000 * 1 -- 1 second

