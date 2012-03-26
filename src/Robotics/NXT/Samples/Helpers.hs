{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- | helper method around the NXT library calls
module Robotics.NXT.Samples.Helpers where

import Robotics.NXT
import Robotics.NXT.Sensor.Ultrasonic

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, unless)

import Data.IORef
import Control.Monad.State.Lazy (StateT, get)
import Control.Monad.Trans.Class (lift)


type PollForStop= NXT Bool

type StopSt=StateT PollForStop NXT

-- | reset the NXT brick motors
reset :: [OutputPort] -- ^ the output ports
        -> StopSt()
reset = mapM_ resetMotor

  
-- | reset a motor
resetMotor :: OutputPort -- ^ the output port
        -> StopSt()
resetMotor p= lift $ mapM_ (resetMotorPosition p) [InternalPosition,AbsolutePosition,RelativePosition]

-- | stop the motors on the given port  
stop :: [OutputPort] -> StopSt()
stop ports=lift $ mapM_ (\p->setOutputStateConfirm p 0 [Regulated,Brake] (regulate ports) 0 MotorRunStateIdle 0) ports
       
-- | move motors on the given ports till the limit has been reached or the stop signal sent       
move :: --PollForStop -- ^ the stopping action
        -- -> 
        [OutputPort] -- ^ the output port
        -> OutputPower  -- ^ the power to apply
        -> [TurnRatio] -- ^ the turn ratio between engine
        -> TachoLimit -- ^ the move limit
        -> StopSt()
move ports power ratios limit=pollForStop $ do
        let port1= head ports
        OutputState _ _ _ _ _ _ _ count _ _<-lift $ getOutputState port1
        mapM_ (\(p,r)->lift $ setOutputStateConfirm p power [Regulated,MotorOn] (regulate ports) r MotorRunStateRunning limit) $ zip ports ratios
        when (limit>0) (pollForCount port1 (count+limit))
      
regulate :: [OutputPort] ->  RegulationMode
regulate [_]=RegulationModeIdle
regulate [] =RegulationModeIdle
regulate _ = RegulationModeMotorSync
      
-- | wait for the given motor to have reached the limit        
pollForCount :: --PollForStop -- ^ the stopping action
        -- -> 
        OutputPort -- ^ the output port
        -> TachoLimit -- ^ the limit
        -> StopSt()
pollForCount port limit=pollForStop $ do
        OutputState _ _ _ _ _ state _ count _ _<-lift $ getOutputState port
        when (state/=MotorRunStateIdle && count<limit) (do
                liftIO $ threadDelay 500
                pollForCount port limit
                )       
  
-- | wait for the input value to reach the given value      
pollForScaled :: -- PollForStop -- ^ the stopping action
         -- -> 
         InputPort -- ^ the input port 
         -> ScaledValue -- ^ the value to wait for
         -> StopSt()
pollForScaled port v=pollForStop $ do
      InputValue _ _ _ _ _ _ _ scalV _<-lift $ getInputValues port
      when (scalV==v) ( do
                liftIO $ threadDelay 500
                pollForScaled port v
                ) 

pollForStopIOR :: IORef Bool  -- ^ the stop signal ioref
        -> PollForStop
pollForStopIOR iorC=liftIO $ atomicModifyIORef iorC (\a->(a,a)) 

pollNeverStop :: PollForStop
pollNeverStop = return True

-- | only perform the given action if the user hasn't said stop                
pollForStop :: --PollForStop -- ^ the stopping action
        -- -> 
        StopSt () -- ^ the action to perform 
        -> StopSt ()
pollForStop f=do
        c<-lift =<< get
        when c f
   
forever ::   StopSt () -- ^ the action to perform 
        -> StopSt ()
forever f=do
        c<-lift =<< get
        when c (do
                f
                forever f)
        
pollForUltrasonic :: --PollForStop
                     --               -> 
                        InputPort
                                    -> Measurement
                                    -> StopSt ()
pollForUltrasonic port v=pollForStop $ do
      mM<-lift $ usGetMeasurement port 0
      ok<-case mM of
        Just m->do
                liftIO $ print m
                return $ m<v
        Nothing->return False
      unless ok ( do
                liftIO $ threadDelay 50000
                pollForUltrasonic port v
                )     
                
-- | waits for user to press space, this stops the robot
waitForStop :: IORef Bool-> IO()
waitForStop iorC=do
        c<-getChar
        if c == ' ' then
          do atomicModifyIORef iorC (\ a -> (False, a))
             return ()
          else waitForStop iorC                 