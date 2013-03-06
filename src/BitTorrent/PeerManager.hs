module BitTorrent.PeerManager where

import Control.Concurrent
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char (ord)
import Data.Word
import Network
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import BitTorrent.Types

type PeerM a = StateT PeerState IO a

data PeerState = PeerState
    { peerSocket :: Maybe Socket
    , peerChoked :: Bool
    , peerInterested :: Bool
    , peerAmChoking :: Bool
    , peerAmInterested :: Bool
    }

defaultState :: PeerState
defaultState = PeerState { peerSocket = Nothing
                         , peerChoked = False
                         , peerInterested = False
                         , peerAmChoking = False
                         , peerAmInterested = False
                         }

runPeerMgr :: Metainfo -> [Peer] -> IO ()
runPeerMgr m ps = do
    -- Open connections
    forM_ ps $ \p -> forkIO $ do
        void $ execStateT (runPeer m p) defaultState

    -- Loop until download finishes, computing interests
    forever $ do
        putStrLn "Tick"
        threadDelay $ 1000000 * 1 -- 1 second

runPeer :: Metainfo -> Peer -> PeerM ()
runPeer m p = do
    liftIO $ putStrLn $ "Connecting to " ++ show p

    peerConnect (peerIp p) (peerPort p)
    peerHandshake $ mtInfoHash m
    peerListen

    return ()

peerConnect :: Word32 -> Word16 -> PeerM ()
peerConnect a p = do
    sock <- liftIO $ socket AF_INET Stream defaultProtocol
    liftIO $ connect sock $ addr a p
    modify $ \s -> s { peerSocket = Just sock }
  where
    addr a p = SockAddrInet (PortNum . fromIntegral $ p) a

peerHandshake :: String -> PeerM ()
peerHandshake ih = do
    sock <- fmap peerSocket get
    case sock of
        Just s -> void . liftIO $ send s $
            B.concat $ map B.pack [ [protoHeaderSize]
                                  , map (fromIntegral . ord) protoHeader
                                  , protoReserved
                                  , map (fromIntegral . ord) ih
                                  , map (fromIntegral . ord) protoPeerId ]
        Nothing -> fail "[handshake] Peer not yet connected."
  where
    protoHeaderSize = fromIntegral . length $ protoHeader
    protoHeader = "BitTorrent protocol"
    protoReserved = replicate 8 0
    protoPeerId = "-HT0001-asdefghjasdh"

peerListen :: PeerM ()
peerListen = do
    (Just sock) <- fmap peerSocket get
    forever $ do
        d <- liftIO $ recv sock 4096
        unless (B.null d) (liftIO $ print d)
