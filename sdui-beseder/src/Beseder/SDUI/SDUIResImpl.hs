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

module Beseder.SDUI.SDUIResImpl where

import           Protolude    
import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.SDUI.SDUIRes
import           Beseder.SDUI.SDUIContext 
import           SDUI.Data.SDUIData  
import           Control.Concurrent.STM.TVar

data UIData = UIData 
  { uiCxt :: SDUIContext
  , uiSessionID :: Text
  , uiReqID :: Int
  }

data UI = UI deriving Show
data UIParams = UPParams SDUIContext EntryID

instance TaskPoster m => SDUIRes m UI where
  data  UIInitialized m UI = UIInitialized UIData 
  data  ShowingDyn m UI = ShowingDyn UIData (TVar (Maybe (UIRespReceived m UI -> m ())))
  data  ShowingStatic m UI = ShowingStatic UIData
  data  UIRespReceived m UI = UIRespReceived UIData UIResp
  data  UIShutdown m UI = UIShutdown
  newtype ResPar m UI = MkUI UIParams

  {-
  initUI :: MkResDef m (ResPar m UI) (UIInitialized m UI)

  showDyn :: RequestDef m ShowDyn (UIInitialized m UI) '[ShowingDyn m UI]  
  showDynDyn :: RequestDef m ShowDyn (ShowingDyn m UI) '[ShowingDyn m UI]  
  showDynStatic :: RequestDef m ShowDyn (ShowingStatic m UI) '[ShowingDyn m UI]  
  showDynResp :: RequestDef m ShowDyn (UIRespReceived m UI) '[ShowingDyn m UI]  

  showStatic :: RequestDef m ShowStatic (UIInitialized m UI) '[ShowingStatic m UI]  
  showStaticDyn :: RequestDef m ShowStatic (ShowingDyn m UI) '[ShowingStatic m UI]  
  showStaticStatic :: RequestDef m ShowStatic (ShowingStatic m UI) '[ShowingStatic m UI]  
  showStaticResp :: RequestDef m ShowStatic (UIRespReceived m UI) '[ShowingStatic m UI]  

  shutdownUI :: RequestDef m ShutdownUI (UIInitialized m UI) '[UIShutdown m UI]  
  shutdownUIDyn :: RequestDef m ShutdownUI (ShowingDyn m UI) '[UIShutdown m UI]  
  shutdownUIStatic :: RequestDef m ShutdownUI (ShowingStatic m UI) '[UIShutdown m UI]  
  shutdownUIResp :: RequestDef m ShutdownUI (UIRespReceived m UI) '[UIShutdown m UI]  

  uiTransition :: TransitionDef m (ShowingDyn m UI) '[UIRespReceived m UI]

  termShutdown :: TermDef m (UIShutdown m UI)

  _uiResp :: UIRespReceived m UI -> m UIResp
  -}

type  UIDyn m = StShowingDyn m UI "ui"
type  UIStatic m = StShowingStatic m UI "ui"
  