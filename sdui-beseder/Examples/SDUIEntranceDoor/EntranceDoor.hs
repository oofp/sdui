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

module  EntranceDoor where

import           Protolude                    hiding (Product, Any, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.ControlData
import           Beseder.Resources.Timer
import           Beseder.Resources.Monitor
import           Beseder.Resources.Switch
import           Beseder.Resources.Comm
import           Beseder.Resources.Monitor.EventMonitorRes 
import           Beseder.SDUI.Resources.UIEvMonitor            
import           Beseder.SDUI.Resources.UIBinMonitor            
import           Beseder.SDUI.Resources.UIBinSwitch            
import           Data.String 
import           Beseder.Misc.Misc
import           Beseder.SDUI.SDUIContext
import           SDUI.Data.SDUIData
import           Beseder.SDUI.SDUIRes
import           Beseder.SDUI.SDUIResImpl
import           Beseder.SDUI.SDUIHelper
import           EntranceDoorHnd
import           GHC.Exts (Any)    


type InitState m doorSw monRes fobMon
    = '[  ( StBinSwitchOff m doorSw "door",
          ( StBinMonitorOff m monRes "inDet", 
          ( StBinMonitorOff m monRes "outDet",  
          ( StWaitForEvent  m fobMon "fobReader")))) 
       ]

fobRes :: SDUIContext ->Beseder.Resources.Monitor.EventMonitorRes.ResPar TaskQ UIEvMonitor
fobRes ctx = MkUIEvMonitor (UIParams ctx (EntryID "fob") (EntryTitle "Fob Reader"))

inRes :: SDUIContext -> Beseder.SDUI.Resources.UIBinMonitor.ResPar TaskQ UIBinMonitor
inRes ctx = MkUIMonitor (UIParams ctx (EntryID "inRes") (EntryTitle "Inner Proximity Sensor"))

outRes :: SDUIContext -> Beseder.SDUI.Resources.UIBinMonitor.ResPar TaskQ UIBinMonitor
outRes ctx = MkUIMonitor (UIParams ctx (EntryID "outRes") (EntryTitle "External Proximity Sensor"))

doorRes :: SDUIContext -> Beseder.SDUI.Resources.UIBinSwitch.ResPar TaskQ UIBinSwitch
doorRes ctx = MkUISwitch (UIParams ctx (EntryID "door") (EntryTitle "Door opener"))

doorApp :: SDUIContext -> STransData TaskQ NoSplitter _ ()
doorApp ctx = do
    newRes #door (doorRes ctx)
    newRes #inDet (inRes ctx)
    newRes #outDet (outRes ctx)
    newRes #fobReader (fobRes ctx)
    newRes #gt TimerRes
    invoke #gt (StartTimer 360000)
    try @("gt" :? IsTimerArmed) (doorHandler 5)
    termAndClearAllResources    

mkSTransDataTypeAny "doorApp" "DoorApp"

-- :kind! Eval (DoorApp NoSplitter '[()])
-- :kind!  ValidateSteps '[] DoorApp NoSplitter '[()]

-- :kind! Eval (DoorHandler FobReaderAlive (InitState IO () ()))
-- :kind! Eval (DoorHandlerCon NoSplitter (InitState IO () ()))
-- :kind!  ValidateSteps '[] DoorHandler FobReaderAlive (InitState IO () ())
-- :kind!  ValidateSteps '[] DoorHandlerCon FobReaderAlive (InitState IO () ())
-- :kind! StateDiagramSym  DoorHandlerCon (InitState IO () ())

runUiDoor ctx = runAsyncData $ doorApp ctx