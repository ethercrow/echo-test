module Main where

import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.IORef

import Node

data NodeConfig = NodeConfig

type NodeM = ReaderT NodeConfig IO

runNode :: NodeConfig -> NodeM a -> IO a
runNode = flip runReaderT

loop :: IORef NodeState -> NodeM ()
loop stateRef = do
    state <- liftIO $ readIORef stateRef
    liftIO $ print state

main :: IO ()
main = do
    stateRef <- newIORef Starting
    runNode NodeConfig . forever $ do
        loop stateRef
        liftIO $ threadDelay oneSecond

oneSecond :: Int
oneSecond = 1000000