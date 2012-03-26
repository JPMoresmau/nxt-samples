{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- | the alligator moves towards a detected target and bites...
-- 
module Main where

import Robotics.NXT
import Robotics.NXT.Samples.Helpers
import System.Environment (getArgs)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent

import Robotics.NXT.Sensor.Ultrasonic
import Control.Monad.State.Lazy (evalStateT)
import Data.IORef (newIORef)
import System.IO (hSetBuffering, stdin, stdout, BufferMode(NoBuffering))

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
                let poll=pollForStopIOR iorC
                usInit Four
                evalStateT (do
                        reset [A]
                        forever (do 
                                pollForUltrasonic Four 60
                                move [B,C] (-100) [0,0] 800
                                bite A
                               )
                        ) poll
                usSetMode Four Off
                )
                
                
bite :: OutputPort -> StopSt ()
bite port=do
        move [port] 75 [0] 10
        reset [port]
        liftIO $ threadDelay 20000
        move [port] (-75) [0] 5  
        reset [port]
                
           