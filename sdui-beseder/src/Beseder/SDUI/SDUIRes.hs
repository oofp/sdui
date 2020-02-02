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

module Beseder.SDUI.SDUIRes where

import           Protolude    
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Resources.ResourceDef
import           SDUI.Data.SDUIData  

newtype ShowDyn = ShowDyn UICard deriving (Eq, Show)
newtype ShowStatic = ShowStatic UICard deriving (Eq, Show)
data ShutdownUI = ShutdownUI deriving (Eq, Show)
  
instance GetInstance ShutdownUI where getInstance = ShutdownUI

class Monad m => SDUIRes m res where
  data  UIInitialized m res 
  data  ShowingDyn m res 
  data  ShowingStatic m res 
  data  UIRespReceived m res 
  data  UIShutdown m res 
  data  ResPar m res 

  initUI :: MkResDef m (ResPar m res) (UIInitialized m res)

  showDyn :: RequestDef m ShowDyn (UIInitialized m res) '[ShowingDyn m res]  
  showDynDyn :: RequestDef m ShowDyn (ShowingDyn m res) '[ShowingDyn m res]  
  showDynStatic :: RequestDef m ShowDyn (ShowingStatic m res) '[ShowingDyn m res]  
  showDynResp :: RequestDef m ShowDyn (UIRespReceived m res) '[ShowingDyn m res]  

  showStatic :: RequestDef m ShowStatic (UIInitialized m res) '[ShowingStatic m res]  
  showStaticDyn :: RequestDef m ShowStatic (ShowingDyn m res) '[ShowingStatic m res]  
  showStaticStatic :: RequestDef m ShowStatic (ShowingStatic m res) '[ShowingStatic m res]  
  showStaticResp :: RequestDef m ShowStatic (UIRespReceived m res) '[ShowingStatic m res]  

  shutdownUI :: RequestDef m ShutdownUI (UIInitialized m res) '[UIShutdown m res]  
  shutdownUIDyn :: RequestDef m ShutdownUI (ShowingDyn m res) '[UIShutdown m res]  
  shutdownUIStatic :: RequestDef m ShutdownUI (ShowingStatic m res) '[UIShutdown m res]  
  shutdownUIResp :: RequestDef m ShutdownUI (UIRespReceived m res) '[UIShutdown m res]  

  uiTransition :: TransitionDef m (ShowingDyn m res) '[UIRespReceived m res]

  termShutdown :: TermDef m (UIShutdown m res)

  _uiResp :: UIRespReceived m res -> m UIResp
  
buildRes ''SDUIRes

type instance TermRequest (StShowingDyn m res name) = ShutdownUI

uiResp :: SDUIRes m res => StUIRespReceived m res name -> m UIResp
uiResp (St uiRespReceived) = _uiResp uiRespReceived

 