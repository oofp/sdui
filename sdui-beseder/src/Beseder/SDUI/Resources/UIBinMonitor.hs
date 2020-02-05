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

module Beseder.SDUI.Resources.UIBinMonitor  where

import           Protolude    
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.SDUI.SDUIResImpl
import           Beseder.SDUI.SDUIRes
import           Beseder.SDUI.SDUIHelper
import           Beseder.Resources.Monitor
--import           Beseder.Resources.State.BinarySwitchRes

data UIBinMonitor = UIBinMonitor deriving (Eq, Show)

monitorOnUI = showButtons "Currently ON" ["Click to switch off"]
monitorOffUI = showButtons "Currently OFF" ["Click to switch on"]
monitorStoppedUI = showNotice "Monitor stopped"

instance (TaskPoster m, SDUIRes m UI) => BinaryMonitorProv m UIBinMonitor where
  newtype  BinMonitorOn m UIBinMonitor = BinMonitorOn (UIDyn m) 
  newtype  BinMonitorOff m UIBinMonitor = BinMonitorOff (UIDyn m)
  newtype  BinMonitorStopped m UIBinMonitor = BinMonitorStopped (UIStatic m)

  newtype ResPar m UIBinMonitor = MkUIMonitor UIParams 

  createBinaryMonitor param = newResFromUI monitorOffUI param
  stopBinaryMonitorOn StopMonitor (BinMonitorOn uiRes) = reqFromUI monitorStoppedUI uiRes
  stopBinaryMonitorOff StopMonitor (BinMonitorOff uiRes) = reqFromUI monitorStoppedUI uiRes
  onOffTransition (BinMonitorOn uiRes) cbFunc = transFromUI1 uiRes monitorOffUI cbFunc 
  offOnTransition (BinMonitorOff uiRes) cbFunc = transFromUI1 uiRes monitorOnUI cbFunc 

  termBinaryMonitor (BinMonitorStopped uiRes) = termFromUI uiRes

