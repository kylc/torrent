module BitTorrent.PeerManager where

import Control.Concurrent
import Control.Monad
import Network
import Network.Socket

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
    putStrLn $ "Connecting to " ++ show p

    let addr = SockAddrInet (PortNum . fromIntegral $ peerPort p) (peerIp p)
    sock <- socket AF_INET Stream defaultProtocol
    connect sock addr
    handshake sock
    str <- recv sock 1

    return ()

handshake :: Socket -> IO Int
handshake s = send s "19BitTorrent protocol00000000123456789123456789012345678901234567890"
