{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell        #-}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module  EntranceDoorHnd where

import           Protolude                    hiding (Product, Any, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.ControlData
import           Beseder.Resources.Timer
import           Beseder.Resources.Monitor
import           Beseder.Resources.Switch
import           Beseder.Resources.Monitor.EventMonitorRes 
import           Data.String 
import           GHC.Exts (Any)    

type InvalidConditions =
  ("doorTimer" :? IsTimerTriggered 
  :|| ("door" :? IsBinSwitchOn :&& (Not ("doorTimer" :? IsTimerArmed))) 
  :|| ("door" :? IsBinSwitchOff :&& ("inDet" :? IsBinMonitorOn)) 
  )

doorHandler :: forall sp m. Int -> STransData m sp _ ()
doorHandler doorTimeoutSec = 
  handleEvents $ do
    on @("fobReader" :? IsEventReceived) $ do 
      invoke #fobReader AckEvent
      openDoorIfClosed doorTimeoutSec       
    on @("inDet" :? IsBinMonitorOn) $ do 
      openDoorIfClosed doorTimeoutSec    
    on @("doorTimer" :? IsTimerTriggered) $ do 
      onOrElse @("inDet" :? IsBinMonitorOn :|| "outDet" :? IsBinMonitorOn)
        (restartTimer doorTimeoutSec)
        closeDoor        
    label #allStates
    assert @(Not InvalidConditions)    

openDoorIfClosed :: Int -> STransData m sp _ ()     
openDoorIfClosed doorTimeoutSec = do
  on @("door" :? IsBinSwitchOff) $ do
    invoke #door TurnOn -- forget to open to demo assertion
    newRes #doorTimer TimerRes
    invoke #doorTimer (StartTimer doorTimeoutSec)
    --invoke #door TurnOn -- to demo validation

closeDoor :: STransData m sp _ () 
closeDoor = do
  clear #doorTimer   
  invoke #door TurnOff
  label #doorClosed

restartTimer :: Int -> STransData m sp _ () 
restartTimer doorTimeoutSec = do
  clear #doorTimer
  newRes #doorTimer TimerRes
  invoke #doorTimer (StartTimer doorTimeoutSec)


mkSTransDataTypeAny "doorHandler" "DoorHandler"
-- :kind!  ValidateSteps '[] DoorHandler NoSplitter InitialState
-- :kind! StateDiagramSym  DoorHandler InitialState
-- :kind! ShowLabel' "doorClosed" DoorHandler NoSplitter InitialState 
-- :kind! ShowLabel' "allState" DoorHandler NoSplitter InitialState 


type InitialState = InitState Any Any Any Any

type InitState m doorSw monRes fobMon
    = '[  ( StBinSwitchOff m doorSw "door",
          ( StBinMonitorOff m monRes "inDet", 
          ( StBinMonitorOff m monRes "outDet",  
          ( StWaitForEvent  m fobMon "fobReader")))) 
       ]

