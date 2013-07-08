module Node 
    ( NodeId (..)
    , NodeState (..)
    , NodeMessage (..)
    , NodeMessagePayload (..)
    , NodeConfig (..)
    , Mailbox
    , processMessage
    , processTimeout
    ) where

import qualified Data.Vector as V

type Mailbox = V.Vector NodeMessage

newtype NodeId = NodeId Int
    deriving (Eq, Show)

data NodeState
    = Starting
    | Master
    | Slave {
          _masterId :: !NodeId
        , _timeSinceMastersReply :: !Int
        }
    | Electing {
          _time :: !Int
        }
    | WaitingForKing {
          _time :: !Int
        }
    deriving (Eq, Show)

data NodeMessage = NodeMessage {
    from :: NodeId
  , to :: NodeId
  , payload :: NodeMessagePayload
} deriving (Eq, Show)

data NodeMessagePayload
    = Alive
    | Fine
    | Ping
    | King
    deriving (Eq, Show)


data NodeConfig = NodeConfig {
    cfgMaxNodeId :: !Int
  , cfgPingInterval :: !Int
} deriving (Eq, Show)

cfgSlaveTimeoutInterval :: NodeConfig -> Int
cfgSlaveTimeoutInterval = (4 *) . cfgPingInterval


processMessage :: NodeId -> NodeState -> NodeMessage
    -> (NodeState, Mailbox)
processMessage selfId selfState (NodeMessage { to = dstId })
    | selfId /= dstId = (selfState, V.empty) -- self is not recipient, ignoring
processMessage i Master (NodeMessage x _ Ping) = (Master, V.singleton $ NodeMessage i x King)
processMessage _ _ (NodeMessage x _ King) = (Slave x 0, V.empty)
processMessage i s (NodeMessage x _ Alive) = (s, V.singleton $ NodeMessage i x Fine)
processMessage _ (Electing _) (NodeMessage _ _ Fine) = (WaitingForKing 0, V.empty)
processMessage _ s _ = (s, V.empty)


processTimeout :: NodeId -> NodeConfig -> NodeState -> (NodeState, Mailbox)
processTimeout i@(NodeId x) (NodeConfig maxNodeId _) Starting =
    (Electing 0, V.fromList $ map (\m -> NodeMessage i (NodeId m) Alive) [(x+1) .. maxNodeId])

processTimeout i@(NodeId x) (NodeConfig maxNodeId pingInterval) (Electing t)
    | t == pingInterval =
        (Master, V.fromList $ map (\m -> NodeMessage i (NodeId m) King) $ filter (/= x) [0 .. maxNodeId])
processTimeout _ _ (Electing t) = (Electing (t + 1), V.empty)

processTimeout i cfg@(NodeConfig _ pingInterval) (WaitingForKing t)
    | t == pingInterval = processTimeout i cfg Starting
processTimeout _ cfg (Slave _ t)
    | t == cfgSlaveTimeoutInterval cfg = (Electing 0, V.empty)
processTimeout i (NodeConfig pingInterval _) (Slave m t) =
    (Slave m (t + 1),
     if t `rem` pingInterval == 0
     then V.singleton $ NodeMessage i m Ping
     else V.empty)
processTimeout _ _ s = (s, V.empty)