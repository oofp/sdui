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
import           Beseder.Base.Common
import           Beseder.SDUI.SDUIRes
import           Beseder.SDUI.SDUIContext 
import           SDUI.Data.SDUIData  
import           Control.Concurrent.STM.TVar

data UIData = UIData 
  { uiCxt :: SDUIContext
  , uiSessionID :: Text
  , uiReqID :: TVar Int
  }

data UI = UI deriving Show
data UIParams = UIParams SDUIContext EntryID EntryTitle

instance TaskPoster m => SDUIRes m UI where
  data  UIInitialized m UI = UIInitialized UIData Text 
  data  ShowingDyn m UI = ShowingDyn UIData 
  data  ShowingStatic m UI = ShowingStatic UIData
  data  UIRespReceived m UI = UIRespReceived UIData UIResp
  data  UIShutdown m UI = UIShutdown
  data ResPar m UI = MkUI UIParams 

  initUI (MkUI (UIParams ctx (EntryID entryID) (EntryTitle title))) = do 
    reqID <- liftIO $ newTVarIO 0
    return (UIInitialized (UIData ctx entryID reqID) title)
  showDyn (ShowDyn uiCard) (UIInitialized uiData title) = do 
    startSession uiData title uiCard
    return $ variantFromValue (ShowingDyn uiData) 

  showDynDyn (ShowDyn uiCard) (ShowingDyn uiData) = do
    showUICard uiData uiCard
    return $ variantFromValue (ShowingDyn uiData) 

  showDynStatic (ShowDyn uiCard) (ShowingStatic uiData) = do
    showUICard uiData uiCard
    return $ variantFromValue (ShowingDyn uiData) 

  showDynResp (ShowDyn uiCard) (UIRespReceived uiData _) = do
    showUICard uiData uiCard
    return $ variantFromValue (ShowingDyn uiData) 

  showStatic (ShowStatic uiCard) (UIInitialized uiData title) = do 
    startSession uiData title uiCard
    return $ variantFromValue (ShowingStatic uiData) 

  showStaticDyn (ShowStatic uiCard) (ShowingDyn uiData) = do
    showUICard uiData uiCard
    return $ variantFromValue (ShowingStatic uiData) 

  showStaticStatic (ShowStatic uiCard) (ShowingStatic uiData) = do
    showUICard uiData uiCard
    return $ variantFromValue (ShowingStatic uiData) 

  showStaticResp (ShowStatic uiCard) (UIRespReceived uiData _) = do
    showUICard uiData uiCard
    return $ variantFromValue (ShowingStatic uiData) 

  uiTransition (ShowingDyn uiData) cb = do
    let cxt = uiCxt uiData
    taskPoster <- getTaskPoster 
    liftIO $ setListener cxt (EntryID (uiSessionID uiData)) 
      (\(ClientResp _entryID (ReqID respReqID) uiRsp) -> 
        taskPoster $ do
          reqID <- liftIO $ atomically $ readTVar (uiReqID uiData)
          when (reqID == respReqID) (cb $ variantFromValue (UIRespReceived uiData uiRsp))
          return True) 

  shutdownUI ShutdownUI  (UIInitialized _uiData _) = return (variantFromValue UIShutdown)  
  shutdownUIDyn ShutdownUI  (ShowingDyn uiData) = shutdownUIData uiData 
  shutdownUIStatic ShutdownUI  (ShowingStatic uiData) = shutdownUIData uiData
  shutdownUIResp ShutdownUI  (UIRespReceived uiData _) = shutdownUIData uiData

  termShutdown UIShutdown = return ()

  _uiResp (UIRespReceived _uiData uiRsp) = return uiRsp 

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
  
startSession :: TaskPoster m => UIData -> Text -> UICard -> m ()
startSession uiData title uiCard = liftIO $ do 
  senderFunc (uiCxt uiData) (CreateEntry (EntryID (uiSessionID uiData)) (EntryTitle title) (ReqID 0) uiCard)  

showUICard :: TaskPoster m => UIData -> UICard -> m ()
showUICard uiData uiCard = liftIO $ do
  nextReqID <- liftIO $ atomically $ do
    reqID <- readTVar (uiReqID uiData)
    let newReqID = reqID + 1
    writeTVar (uiReqID uiData) newReqID
    return newReqID
  senderFunc (uiCxt uiData) (ReplaceEntry (EntryID (uiSessionID uiData)) (ReqID nextReqID) uiCard)  

shutdownUIData :: MonadIO m => UIData -> m (V '[UIShutdown m UI])
shutdownUIData uiData = liftIO $ do
  removeListener (uiCxt uiData) (EntryID (uiSessionID uiData))
  return (variantFromValue UIShutdown)  