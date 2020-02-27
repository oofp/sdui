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

module Beseder.SDUI.UIResources.InputRes where

import           Protolude    
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Resources.ResourceDef
  
data StopInput = StopInput deriving (Eq,Show)
instance GetInstance StopInput where getInstance = StopInput

class Monad m => InputRes m res where
  data  Inputing m res 
  data  CollectedText m res 
  data  CollectedNum m res 
  data  CollectedBool m res 
  data  InputStopped m res 
  data  InputCompleted m res 
  data  ResPar m res 

  initUI :: MkResDef m (ResPar m res) (Inputing m res)

  inputingTransition :: TransitionDef m (Inputing m res) '[CollectedText m res,CollectedNum m res, CollectedBool m res,InputCompleted m res]
  textTransition :: TransitionDef m (CollectedText m res) '[Inputing m res,InputCompleted m res]
  numTransition :: TransitionDef m (CollectedNum m res) '[Inputing m res,InputCompleted m res]
  boolTransition :: TransitionDef m (CollectedBool m res) '[Inputing m res,InputCompleted m res]

  stopInputting :: RequestDef m StopInput (Inputing m res) '[InputStopped m res]  
  stopText :: RequestDef m StopInput (CollectedText m res) '[InputStopped m res]  
  stopNum :: RequestDef m StopInput (CollectedNum m res) '[InputStopped m res]  
  stopBool :: RequestDef m StopInput (CollectedBool m res) '[InputStopped m res]  
 
  termCompleted :: TermDef m (InputCompleted m res)
  termStopped :: TermDef m (InputStopped m res)

  _collectedText :: CollectedText m res -> m Text
  _collectedNum :: CollectedNum m res -> m Int
  _collectedBool :: CollectedBool m res -> m Bool
  
buildRes ''InputRes

collectedText :: InputRes m res  => StCollectedText m res name -> m Text
collectedText (St stData) = _collectedText stData

collectedNum :: InputRes m res  => StCollectedNum m res name -> m Int
collectedNum(St stData) = _collectedNum stData

collectedBool :: InputRes m res  => StCollectedBool m res name -> m Bool
collectedBool (St stData) = _collectedBool stData

type instance TermRequest (StInputing m res name) = StopInput
type instance TermRequest (StCollectedText m res name) = StopInput
type instance TermRequest (StCollectedNum m res name) = StopInput
type instance TermRequest (StCollectedBool m res name) = StopInput

