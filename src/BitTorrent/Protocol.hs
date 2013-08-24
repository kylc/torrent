module BitTorrent.Protocol where

type ProtocolName = B.ByteString
type ProtocolExt = B.ByteString
type PeerID = B.ByteString

data Message = KeepAlive
                 | Handshake ProtocolName ProtocolExt Hash PeerID
                 | Choke
                 | Unchoke
                 | Interested
                 | NotInterested
                 | Have Int
                 | Bitfield B.ByteString
                 | Request Int Int Int
                 | Piece Int Int B.ByteString
                 | Cancel Int Int Int
                 | Port Int
    deriving (Eq, Show)
