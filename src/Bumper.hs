{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- | moves a robot using two tracks on engines B & C
-- the robot moves tills the tactile sensor is pressed, then it reverse a little, turns and start again
module Main where

import Robotics.NXT

import Robotics.NXT.Samples.Helpers

import System.Environment (getArgs)
import Control.Concurrent (threadDelay,forkIO)
import Control.Monad.IO.Class (liftIO)

import Data.IORef
import System.IO
import Control.Monad.State.Lazy (evalStateT)
import Control.Monad.Trans.Class (lift)

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
                evalStateT (do
                        reset [B,C]
                        forever loop 
                        reset [B,C]
                        ) (pollForStopIOR iorC)
                liftIO $ threadDelay 1000000  -- wait before killing everything probably not needed after reset   
                )
        --killThread tid 



-- | the main loop for the robot
loop :: --PollForStop -- ^ the stopping action
        -- -> 
        StopSt()
loop = do
  move  [B,C] 75 [0,0] 0 -- move forever
  lift $ setInputModeConfirm One Switch BooleanMode -- set the sensor on port One to switch mode
  pollForScaled  One 0 -- wait for sensor to be triggered
  stop [B,C] -- stop
  move [B,C] (-75) [0,0] 360 -- reverse
  move [B,C] (-75) [100,-100] 360 -- turn
  stop [B,C] -- stop
  

        