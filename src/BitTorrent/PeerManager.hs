module BitTorrent.PeerManager
    ( runPeerMgr
    ) where

import Control.Concurrent
import Control.Concurrent.Delay
import Control.Monad.Reader
import Control.Monad.State

import BitTorrent.Peer
import BitTorrent.Types

runPeerMgr :: Metainfo -> [Peer] -> IO ()
runPeerMgr m ps = do
    -- Open connections
    forM_ ps $ \p -> forkIO $
        let st = defaultPeerState $ length (mtPieces m)
        in void $ execStateT (runReaderT (runPeer p) m) st

    -- Loop until download finishes, computing interests
    forever $ do
        putStrLn "Tick"
        delaySeconds 1
