module Main where

import qualified Data.Vector as V

import Test.HUnit
import qualified Test.Framework as TF
import Test.Framework.Providers.HUnit

import Node
import Simulation

simulateNSteps :: Int -> TestCluster -> TestCluster
simulateNSteps n cluster = iterate simulateStep cluster !! n

freshCluster :: TestCluster
freshCluster = TestCluster (NodeConfig 3 4) . V.fromList $
    [ TestNode (NodeId 0) Starting V.empty
    , TestNode (NodeId 1) Starting V.empty
    , TestNode (NodeId 2) Starting V.empty
    ]

electingCluster :: TestCluster
electingCluster = TestCluster (NodeConfig 3 4) . V.fromList $
    [ TestNode (NodeId 0) (WaitingForKing 0) V.empty
    , TestNode (NodeId 1) (WaitingForKing 0) V.empty
    , TestNode (NodeId 2) (Electing 0) V.empty
    ]

stableCluster :: TestCluster
stableCluster = TestCluster (NodeConfig 3 4) . V.fromList $
    [ TestNode (NodeId 0) (Slave (NodeId 2) 0) V.empty
    , TestNode (NodeId 1) (Slave (NodeId 2) 0) V.empty
    , TestNode (NodeId 2) Master V.empty
    ]

stepCount :: Int
stepCount = 667

testEquilibriumWithoutFailures :: Assertion
testEquilibriumWithoutFailures = assertEqual "" stableCluster evolvedCluster
    where evolvedCluster = simulateNSteps stepCount stableCluster

testElectionWithoutFailures :: Assertion
testElectionWithoutFailures = assertEqual "" stableCluster evolvedCluster
    where evolvedCluster = simulateNSteps stepCount freshCluster

testStartup = assertEqual "" electingCluster (simulateStep freshCluster)

main :: IO ()
main = TF.defaultMain . map (testCase "") $
    [ testEquilibriumWithoutFailures
    , testStartup
    , testElectionWithoutFailures
    ]