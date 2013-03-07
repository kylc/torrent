module BitTorrent.Peer
    ( runPeer
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char (ord)
import Data.Word
import Network
import Network.Socket hiding (KeepAlive, send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Binary as A
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

import BitTorrent.Types

import Debug.Trace

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

peerHandshake :: B.ByteString -> PeerM ()
peerHandshake ih = do
    sock <- fmap peerSocket get
    case sock of
        Just s -> void . liftIO $ send s $
            B.concat [ B.pack [protoHeaderSize]
                     , strToBS protoHeader
                     , B.pack protoReserved
                     , ih
                     , strToBS protoPeerId ]
        Nothing -> fail "[handshake] Peer not yet connected."
  where
    protoHeaderSize = fromIntegral . length $ protoHeader
    protoHeader = "BitTorrent protocol"
    protoReserved = replicate 8 0
    protoPeerId = "-HT0001-asdefghjasdh"
    strToBS = B.pack . map (fromIntegral . ord)

peerListen :: PeerM ()
peerListen = do
    (Just sock) <- fmap peerSocket get
    (is, os) <- liftIO $ Streams.socketToStreams sock
    forever $ do
        p <- parser
        m <- liftIO $ Streams.parseFromStream p is
        trace (show m) $ return m
        modify $ \s -> s { peerHandshaken = True } -- TODO
  where
    parser = do
        shaken <- fmap peerHandshaken get
        return $ if shaken
                     then parseMessage
                     else parseHandshake

parseMessage :: A.Parser Message
parseMessage = do
    len <- A.anyWord8
    if len == 0
        then return KeepAlive
        else parseMessage' $ fromIntegral len
  where
    parseMessage' len = do
        id <- A.anyWord8
        case id of
            0 -> return Choke
            1 -> return Unchoke
            2 -> return Interested
            3 -> return NotInterested
            4 -> Have <$> w32
            5 -> Bitfield <$> A.take (len - 1)
            6 -> Request <$> w32 <*> w32 <*> w32
            7 -> Piece <$> w32 <*> w32 <*> A.take (len - 9)
            8 -> Cancel <$> w32 <*> w32 <*> w32
            9 -> Port <$> w32
            _ -> fail $ "Invalid message id: " ++ show id
    w32 = fromIntegral <$> A.anyWord32be

parseHandshake :: A.Parser Message
parseHandshake =
    Handshake <$> (A.anyWord8 >>= A.take . fromIntegral)
              <*> A.take 8 <*> A.take 20 <*> A.take 20

