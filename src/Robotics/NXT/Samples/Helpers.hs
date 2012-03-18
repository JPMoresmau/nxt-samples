-- | helper method around the NXT library calls
module Robotics.NXT.Samples.Helpers where

import Robotics.NXT

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)

import Data.IORef


type PollForStop= NXT Bool

-- | reset the NXT brick motors
reset :: [OutputPort] -- ^ the output ports
        -> NXT()
reset = mapM_ resetMotor

  
-- | reset a motor
resetMotor :: OutputPort -- ^ the output port
        -> NXT()
resetMotor p= mapM_ (resetMotorPosition p) [InternalPosition,AbsolutePosition,RelativePosition]

-- | stop the motors on the given port  
stop :: [OutputPort] -> NXT()
stop ports=mapM_ (\p->setOutputStateConfirm p 0 [Regulated,Brake] (regulate ports) 0 MotorRunStateIdle 0) ports
       
-- | move motors on the given ports till the limit has been reached or the stop signal sent       
move :: PollForStop -- ^ the stopping action
        -> [OutputPort] -- ^ the output port
        -> OutputPower  -- ^ the power to apply
        -> [TurnRatio] -- ^ the turn ratio between engine
        -> TachoLimit -- ^ the move limit
        -> NXT()
move iorC ports power ratios limit=pollForStop iorC $ do
        let port1= head ports
        OutputState _ _ _ _ _ _ _ count _ _<-getOutputState port1
        mapM_ (\(p,r)->setOutputStateConfirm p power [Regulated,MotorOn] (regulate ports) r MotorRunStateRunning limit) $ zip ports ratios
        when (limit>0) (pollForCount iorC port1 (count+limit))
      
regulate :: [OutputPort] ->  RegulationMode
regulate [_]=RegulationModeIdle
regulate [] =RegulationModeIdle
regulate _ = RegulationModeMotorSync
      
-- | wait for the given motor to have reached the limit        
pollForCount :: PollForStop -- ^ the stopping action
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
pollForScaled :: PollForStop -- ^ the stopping action
         -> InputPort -- ^ the input port 
         -> ScaledValue -- ^ the value to wait for
         -> NXT()
pollForScaled iorC port v=pollForStop iorC $ do
      InputValue _ _ _ _ _ _ _ scalV _<-getInputValues port
      when (scalV==v) ( do
                liftIO $ threadDelay 500
                pollForScaled iorC port v
                ) 

pollForStopIOR :: IORef Bool  -- ^ the stop signal ioref
        -> PollForStop
pollForStopIOR iorC=liftIO $ atomicModifyIORef iorC (\a->(a,a)) 

pollNeverStop :: PollForStop
pollNeverStop = return True

-- | only perform the given action if the user hasn't said stop                
pollForStop :: PollForStop -- ^ the stopping action
        -> NXT() -- ^ the action to perform 
        -> NXT()
pollForStop pfs f=do
        c<-pfs
        when c f