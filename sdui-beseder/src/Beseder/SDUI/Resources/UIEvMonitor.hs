{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}

module Beseder.SDUI.Resources.UIEvMonitor  where

import           Protolude    
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.SDUI.SDUIResImpl
import           Beseder.SDUI.SDUIRes
import           Beseder.SDUI.SDUIHelper
import           Beseder.Resources.Monitor.EventMonitorRes 

data UIEvMonitor = UIEvMonitor deriving (Eq, Show)

monitorWaitingUI = showButtons "Monitor events" ["Click to trigger event"]
monitorStoppedUI = showNotice "Monitor stopped"
monitorReceivedUI = showNotice "Wait for ack"

instance (TaskPoster m, SDUIRes m UI) => EventMonitorProv m UIEvMonitor where
  newtype  WaitForEvent m UIEvMonitor = WaitForEvent (UIDyn m) 
  newtype  EventReceived m UIEvMonitor = EventReceived (UIStatic m)
  newtype  EvMonitorStopped m UIEvMonitor = EvMonitorStopped (UIStatic m)

  newtype ResPar m UIEvMonitor = MkUIEvMonitor UIParams 

  createEvMonitor param = newResFromUI monitorWaitingUI param
  stopEvMonitorWait StopEvMonitor (WaitForEvent uiRes) = reqFromUI monitorStoppedUI uiRes
  stopEvMonitorReceived StopEvMonitor (EventReceived uiRes) = reqFromUI monitorStoppedUI uiRes
  ackEvent AckEvent (EventReceived uiRes) = reqFromUI monitorWaitingUI uiRes
  evTransition (WaitForEvent uiRes) cbFunc = transFromUI1 uiRes monitorReceivedUI cbFunc 

  termEvMonitor (EvMonitorStopped uiRes) = termFromUI uiRes

