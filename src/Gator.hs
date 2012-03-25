-- | the alligator moves towards a detected target and bites...
-- 
module Main where

import Robotics.NXT
import Robotics.NXT.Samples.Helpers
import System.Environment (getArgs)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent

import Robotics.NXT.Sensor.Ultrasonic

main :: IO()
main = do
        (device:_)<-getArgs
        withNXT device (do
                reset [A]
                usInit Four
                let poll=pollNeverStop
                pollForUltrasonic poll Four 60
                move poll [B,C] (-75) [0,0] 800
                bite poll A
                usSetMode Four Off
                )
                
                
bite :: PollForStop -> OutputPort -> NXT()
bite poll port=do
        move poll [port] 75 [0] 10
        reset [port]
        liftIO $ threadDelay 20000
        move poll [port] (-75) [0] 5  
        reset [port]
                
           