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

module Beseder.SDUI.Resources.UIBinSwitch  where

import           Protolude    
import           Beseder.Base.Common
import           Beseder.SDUI.SDUIResImpl
import           Beseder.SDUI.SDUIRes
import           Beseder.SDUI.SDUIHelper
import           Beseder.Resources.Switch
--import           Beseder.Resources.State.BinarySwitchRes

data UIBinSwitch = UIBinSwitch deriving (Eq, Show)

switchOnUI :: ShowStatic
switchOnUI = showNotice "Open"
switchOffUI :: ShowStatic
switchOffUI = showNotice "Closed"

instance (TaskPoster m, SDUIRes m UI) => BinarySwitchProv m UIBinSwitch where
  newtype  BinSwitchOn m UIBinSwitch  = BinSwitchOn (UIStatic m) 
  newtype  BinSwitchOff m UIBinSwitch  = BinSwitchOff (UIStatic m) 
  newtype ResPar m UIBinSwitch = MkUISwitch UIParams 

  createBinarySwitch param = newResFromUI switchOffUI param

  turnOffBinarySwitch TurnOff (BinSwitchOn uiRes) = reqFromUI switchOffUI uiRes  
  turnOnBinarySwitch TurnOn (BinSwitchOff uiRes) = reqFromUI switchOnUI uiRes
  termOnBinarySwitch (BinSwitchOn uiRes) = termFromUI uiRes
  termOffBinarySwitch (BinSwitchOff uiRes) = termFromUI uiRes
