-- | moves a robot using two tracks on engines B & C
-- the robot moves tills the tactile sensor is pressed, then it reverse a little, turns and start again
module Main where

import Robotics.NXT

import System.Environment (getArgs)
import Control.Concurrent (threadDelay,forkIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, forever)

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
                reset
                forever $ loop iorC
                reset
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
        
-- | reset the NXT brick motors
reset :: NXT()
reset = mapM_ resetMotor [B,C]
  
-- | reset a motor
resetMotor :: OutputPort -- ^ the output port
        -> NXT()
resetMotor p= mapM_ (resetMotorPosition p) [InternalPosition,AbsolutePosition,RelativePosition]
  
-- | the main loop for the robot
loop :: IORef Bool -- ^ the stop flag ioref
        -> NXT()
loop iorC= do
  move iorC [B,C] 75 [0,0] 0 -- move forever
  setInputModeConfirm One Switch BooleanMode -- set the sensor on port One to switch mode
  pollForScaled iorC One 0 -- wait for sensor to be triggered
  stop [B,C] -- stop
  move iorC [B,C] (-75) [0,0] 360 -- reverse
  move iorC [B,C] (-75) [100,-100] 360 -- turn
  stop [B,C] -- stop
  
  
-- | stop the motors on the given port  
stop :: [OutputPort] -> NXT()
stop =mapM_ (\p->setOutputStateConfirm p 0 [Regulated,Brake] RegulationModeMotorSync 0 MotorRunStateIdle 0)
       
-- | move motors on the given ports till the limit has been reached or the stop signal sent       
move :: IORef Bool  -- ^ the stop signal ioref
        -> [OutputPort] -- ^ the output port
        -> OutputPower  -- ^ the power to apply
        -> [TurnRatio] -- ^ the turn ratio between engine
        -> TachoLimit -- ^ the move limit
        -> NXT()
move iorC ports power ratios limit=pollForStop iorC $ do
        let port1= head ports
        OutputState _ _ _ _ _ _ _ count _ _<-getOutputState port1
        mapM_ (\(p,r)->setOutputStateConfirm p power [Regulated,MotorOn] RegulationModeMotorSync r MotorRunStateRunning limit) $ zip ports ratios
        when (limit>0) (pollForCount iorC port1 (count+limit))
      
-- | wait for the given motor to have reached the limit        
pollForCount :: IORef Bool  -- ^ the stop signal ioref
        -> OutputPort -- ^ the output port
        -> TachoLimit -- ^ the limit
        -> NXT()
pollForCount iorC port limit=pollForStop iorC $ do
        OutputState _ _ _ _ _ state _ count _ _<-getOutputState port
        when (state/=MotorRunStateIdle && count<limit) (do
                liftIO $ threadDelay 500
                pollForCount iorC port limit
                )       
  
-- | wait for the input value to reach the given value      
pollForScaled :: IORef Bool -- ^ the stop signal ioref
         -> InputPort -- ^ the input port 
         -> ScaledValue -- ^ the value to wait for
         -> NXT()
pollForScaled iorC port v=pollForStop iorC $ do
      InputValue _ _ _ _ _ _ _ scalV _<-getInputValues port
      when (scalV==v) ( do
                liftIO $ threadDelay 500
                pollForScaled iorC port v
                ) 

-- | only perform the given action if the user hasn't said stop                
pollForStop :: IORef Bool  -- ^ the stop signal ioref
        -> NXT() -- ^ the action to perform 
        -> NXT()
pollForStop iorC f=do
        c<-liftIO $ atomicModifyIORef iorC (\a->(a,a)) 
        when c f
        