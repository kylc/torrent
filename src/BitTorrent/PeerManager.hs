module BitTorrent.PeerManager where

import Control.Concurrent
import Control.Monad
import Network

import BitTorrent.Types

runPeerMgr :: [Peer] -> IO ()
runPeerMgr ps = do
    -- Open connections
    forM_ ps $ forkIO . runPeer

    -- Loop until download finishes, computing interests
    forever $ do
        putStrLn "Tick"
        threadDelay $ 1000000 * 1 -- 1 second

runPeer :: Peer -> IO ()
runPeer p = do
    h <- connectTo (peerIp p) (PortNumber . fromIntegral $ peerPort p)
    print h
