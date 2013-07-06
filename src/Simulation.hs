{-# LANGUAGE ScopedTypeVariables #-}

module Simulation
    ( TestNode(..)
    , TestCluster(..)
    , simulateStep
    ) where

import Node

import qualified Data.Foldable as F
import Data.Monoid ((<>))
import qualified Data.Vector as V

data TestNode = TestNode NodeId NodeState Mailbox
    deriving (Eq, Show)

data TestCluster = TestCluster {
    nodes :: V.Vector TestNode
} deriving Eq

instance Show TestCluster where
    show (TestCluster nodes) = "Cluster:\n" ++ unlines (V.toList (V.map show nodes))

simulateStep :: TestCluster -> TestCluster
simulateStep (TestCluster nodes) = TestCluster $ processAll nodesAfterTimeout
    where (nodesAfterTimeout, messagesAfterTimeout) = simulateTimeout nodes
          processAll :: V.Vector TestNode -> V.Vector TestNode
          processAll nodes =
              let (nodes' :: V.Vector TestNode, mbs') = V.unzip $ V.map processInbox nodes
              in if F.and (fmap V.null mbs')
                 then nodes'
                 else processAll $ putMessagesIntoInboxes (F.msum mbs') nodes'

simulateTimeout :: V.Vector TestNode -> (V.Vector TestNode, Mailbox)
simulateTimeout nodes = (putMessagesIntoInboxes globalOutbox nodes', globalOutbox)
    where globalOutbox = F.msum outboxes
          (nodes', outboxes) = V.unzip $ V.map processTimeout' nodes

processTimeout' :: TestNode -> (TestNode, Mailbox)
processTimeout' (TestNode nid oldState inbox) = (TestNode nid newState inbox, outbox)
    where (newState, outbox) = processTimeout nid oldState 

processInbox :: TestNode -> (TestNode, Mailbox)
processInbox (TestNode nid oldState inbox) = (TestNode nid newState V.empty, outbox)
    where (newState, outbox) = V.foldl go (oldState, V.empty) inbox
          go (curState, curOutbox) msg = 
              let (nextState, moreMessages) = processMessage nid curState msg
              in (nextState, curOutbox <> moreMessages)

putMessagesIntoInboxes :: Mailbox -> V.Vector TestNode -> V.Vector TestNode
putMessagesIntoInboxes messages = V.map takeRelevantMessages
    where takeRelevantMessages (TestNode nid state inbox) =
            TestNode nid state (inbox <> V.filter ((== nid) . to) messages)