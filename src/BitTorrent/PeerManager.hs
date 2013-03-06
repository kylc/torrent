module BitTorrent.PeerManager where

import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Data.Word
import Network
import Network.Socket

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

runPeerMgr :: [Peer] -> IO ()
runPeerMgr ps = do
    -- Open connections
    forM_ ps $ \p -> forkIO $
        void $ execStateT (runPeer p) defaultState

    -- Loop until download finishes, computing interests
    forever $ do
        putStrLn "Tick"
        threadDelay $ 1000000 * 1 -- 1 second

runPeer :: Peer -> PeerM ()
runPeer p = do
    liftIO $ putStrLn $ "Connecting to " ++ show p

    peerConnect (peerIp p) (peerPort p)
    peerHandshake
    -- str <- recv sock 1

    return ()

peerConnect :: Word32 -> Word16 -> PeerM ()
peerConnect a p = do
    sock <- liftIO $ socket AF_INET Stream defaultProtocol
    liftIO $ connect sock $ addr a p
    modify $ \s -> s { peerSocket = Just sock }
  where
    addr a p = SockAddrInet (PortNum . fromIntegral $ p) a

peerHandshake :: PeerM ()
peerHandshake = do
    sock <- fmap peerSocket get
    case sock of
        Just s -> void . liftIO $ send s "19BitTorrent protocol00000000123456789123456789012345678901234567890"
        Nothing -> fail "[handshake] Peer not yet connected."
