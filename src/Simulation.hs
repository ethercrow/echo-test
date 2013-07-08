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
    clusterConfig :: NodeConfig
  , nodes :: V.Vector TestNode
} deriving Eq

instance Show TestCluster where
    show (TestCluster cfg nodes) = "Cluster:\n" ++ unlines (show cfg : V.toList (V.map show nodes))

simulateStep :: TestCluster -> TestCluster
simulateStep (TestCluster cfg nodes) = TestCluster cfg (processAll nodesAfterTimeout)
    where (nodesAfterTimeout, messagesAfterTimeout) = simulateTimeout cfg nodes
          processAll :: V.Vector TestNode -> V.Vector TestNode
          processAll nodes =
              let (nodes' :: V.Vector TestNode, mbs') = V.unzip $ V.map processInbox nodes
              in if F.and (fmap V.null mbs')
                 then nodes'
                 else processAll $ putMessagesIntoInboxes (F.msum mbs') nodes'

simulateTimeout :: NodeConfig -> V.Vector TestNode -> (V.Vector TestNode, Mailbox)
simulateTimeout cfg nodes = (putMessagesIntoInboxes globalOutbox nodes', globalOutbox)
    where globalOutbox = F.msum outboxes
          (nodes', outboxes) = V.unzip $ V.map (processTimeout' cfg) nodes

processTimeout' :: NodeConfig -> TestNode -> (TestNode, Mailbox)
processTimeout' cfg (TestNode nid oldState inbox) = (TestNode nid newState inbox, outbox)
    where (newState, outbox) = processTimeout nid cfg oldState

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