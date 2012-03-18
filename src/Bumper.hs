{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- | moves a robot using two tracks on engines B & C
-- the robot moves tills the tactile sensor is pressed, then it reverse a little, turns and start again
module Main where

import Robotics.NXT

import Robotics.NXT.Samples.Helpers

import System.Environment (getArgs)
import Control.Concurrent (threadDelay,forkIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever)

import Data.IORef
import System.IO

-- | the main method
main :: IO()
main = do
        (device:_)<-getArgs
        iorC<-newIORef True
        forkIO (do
                hSetBuffering stdin NoBuffering
                hSetBuffering stdout NoBuffering -- does not Work on windows
                putStrLn "press space to stop robot"
                waitForStop iorC
                putStrLn "stopping..."
                return () 
                )
        withNXT device (do
                reset [B,C]
                forever $ loop $ pollForStopIOR iorC
                reset [B,C]
                liftIO $ threadDelay 1000000  -- wait before killing everything probably not needed after reset   
                )
        --killThread tid 

-- | waits for user to press space, this stops the robot
waitForStop :: IORef Bool-> IO()
waitForStop iorC=do
        c<-getChar
        if c == ' ' then
          do atomicModifyIORef iorC (\ a -> (False, a))
             return ()
          else waitForStop iorC

-- | the main loop for the robot
loop :: PollForStop -- ^ the stopping action
        -> NXT()
loop iorC= do
  move iorC [B,C] 75 [0,0] 0 -- move forever
  setInputModeConfirm One Switch BooleanMode -- set the sensor on port One to switch mode
  pollForScaled iorC One 0 -- wait for sensor to be triggered
  stop [B,C] -- stop
  move iorC [B,C] (-75) [0,0] 360 -- reverse
  move iorC [B,C] (-75) [100,-100] 360 -- turn
  stop [B,C] -- stop
  

        