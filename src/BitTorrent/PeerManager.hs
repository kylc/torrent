module BitTorrent.PeerManager where

import Control.Concurrent
import Control.Monad
import Data.Word
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

    sock <- peerConnect (peerIp p) (peerPort p)
    handshake sock
    str <- recv sock 1

    return ()

peerConnect :: Word32 -> Word16 -> IO Socket
peerConnect a p = do
    let addr = SockAddrInet (PortNum . fromIntegral $ p) a
    sock <- socket AF_INET Stream defaultProtocol
    connect sock addr
    return sock

handshake :: Socket -> IO Int
handshake s = send s "19BitTorrent protocol00000000123456789123456789012345678901234567890"
