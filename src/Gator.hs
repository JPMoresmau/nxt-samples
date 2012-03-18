
module Main where

import Robotics.NXT
import Robotics.NXT.Samples.Helpers
import System.Environment (getArgs)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent

main :: IO()
main = do
        (device:_)<-getArgs
        withNXT device (do
                reset [A]
                let poll=pollNeverStop
                move poll [A] 75 [0] 10
                liftIO $ threadDelay 200000
                move poll [A] (-75) [0] (8)  
                reset [A]
                )