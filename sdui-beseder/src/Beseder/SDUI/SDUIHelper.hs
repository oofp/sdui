{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}

module Beseder.SDUI.SDUIHelper 
  ( transFromUI
  , transFromUI1
  , transFromBtn2
  , newResFromUI
  , reqFromUI
  , termFromUI
  , showButtons
  , showNotice
  --
  , UIDynData
  , UIStaticData
  , UIStData (..)
  , GetStCard (..)
  , GetFormEntries (..)
  , ReqUI1 (..)
  , transFromUIDynData
  , newResFromUIData
  , uiTrans
  , termUI
  , getTextFromResp 
  , getNumFromResp 
  , getBoolFromResp 
  ) where

import           Protolude
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.SDUI.SDUIResImpl
import qualified Beseder.SDUI.SDUIRes as SDUIRes
import           Beseder.SDUI.SDUIRes
-- import           Beseder.SDUI.SDUIContext
import           SDUI.Data.SDUIData 
import           SDUI.Data.Form 
import           SDUI.Data.Button
import           SDUI.Data.Style
import           SDUI.Data.UIHelper
import           Haskus.Utils.Variant
import           Data.Coerce
import           Data.Text

uiResParams :: (Coercible resParams UIParams, Monad m) => resParams -> m (SDUIRes.ResPar m UI) -- (CardReaderIdle m UICardReader)
uiResParams resParams = do
  let uiParams :: UIParams
      uiParams = coerce resParams
  let uiResPar :: SDUIRes.ResPar m UI
      uiResPar = MkUI uiParams
  return uiResPar

transFromUI :: 
  ( CoerceVar xs ys
  , TaskPoster m
  ) => UIDyn m -> (StUIRespReceived m UI "ui" -> m (V xs)) -> (V ys -> m ()) -> m ()
transFromUI uiDyn respToStates cbFunc =
  void $ next uiDyn (\uiRespReceivedVar -> do
      --uiRsp <- uiResp (variantToValue uiRespReceivedVar)
      let respReceived =  variantToValue uiRespReceivedVar
      nextStates <- respToStates respReceived 
      cbFunc (coerceVar nextStates)
      return True)

transFromUI1 :: 
  ( CoerceVar '[x] '[y]
  , TaskPoster m
  , Request m req (StUIRespReceived m UI "ui") 
  , '[x] ~ ReqResult req (StUIRespReceived m UI "ui") 
  ) => UIDyn m -> req -> (V '[y] -> m ()) -> m ()
transFromUI1 uiDyn req cbFunc =
   transFromUI uiDyn (request req) cbFunc 

bbHandler2 :: 
  ( TaskPoster m
  , LiftVariant (ReqResult req1 (StUIRespReceived m UI "ui")) xs
  , LiftVariant (ReqResult req2 (StUIRespReceived m UI "ui")) xs
  , Request m req1 (StUIRespReceived m UI "ui")
  , Request m req2 (StUIRespReceived m UI "ui")  
  ) => Text -> req1 -> req2 -> StUIRespReceived m UI "ui" -> m (V xs)
bbHandler2 btnResp1 req1 req2 resp = do
  uiRsp <- uiResp resp
  if (uiRsp == ButtonClickedResp btnResp1)
    then liftVariant <$> request req1 resp
    else liftVariant <$> request req2 resp     

bbHandler2' :: 
  ( TaskPoster m
  , '[x1] ~ ReqResult req1 (StUIRespReceived m UI "ui")
  , '[x2] ~ ReqResult req2 (StUIRespReceived m UI "ui")
  , xs ~ '[x1,x2]
  , LiftVariant (ReqResult req1 (StUIRespReceived m UI "ui")) xs
  , LiftVariant (ReqResult req2 (StUIRespReceived m UI "ui")) xs
  , Request m req1 (StUIRespReceived m UI "ui")
  , Request m req2 (StUIRespReceived m UI "ui")  
  ) => Text -> req1 -> req2 -> StUIRespReceived m UI "ui" -> m (V xs)
bbHandler2' = bbHandler2 

transFromBtn2 :: 
  (CoerceVar '[x1, x2] ys, TaskPoster m,
    LiftVariant
      (ReqResult req1 (StUIRespReceived m UI "ui")) '[x1, x2],
    LiftVariant
      (ReqResult req2 (StUIRespReceived m UI "ui")) '[x1, x2],
    Request m req1 (StUIRespReceived m UI "ui"),
    Request m req2 (StUIRespReceived m UI "ui"),
    ReqResult req2 (StUIRespReceived m UI "ui") ~ '[x2],
    ReqResult req1 (StUIRespReceived m UI "ui") ~ '[x1]
  ) => UIDyn m -> Text -> req1 -> req2 -> (V ys -> m ()) -> m ()
transFromBtn2 uiDyn btnResp1 req1 req2 cbFunc =
  transFromUI uiDyn (bbHandler2' btnResp1 req1 req2) cbFunc

newResFromUI :: 
  ( Coercible resParams UIParams
  , TaskPoster m 
  , Request m initUIReq (St (UIInitialized m UI) "ui")
  , ReqResult initUIReq (St (UIInitialized m UI) "ui") ~ '[a]
  , Coercible a initResState) => initUIReq -> resParams -> m initResState
newResFromUI initUIReq resParams = do
  uiResPar <- uiResParams resParams
  uiInitilized <- fmap (stFromName #ui) (mkRes uiResPar)
  uiShowing <- request initUIReq uiInitilized
  return  $ coerce (variantToValue uiShowing) 

reqFromUI :: (Request m uiReq uiRes, CoerceVar (ReqResult uiReq uiRes) ys) => uiReq -> uiRes -> m (V ys) 
reqFromUI uiReq uiRes = do
  newUIState <- request uiReq uiRes
  return $ coerceVar newUIState 

termFromUI :: (Request m ShutdownUI state, TaskPoster m,
                ReqResult ShutdownUI state ~ '[StUIShutdown m UI "ui"]) =>
              state -> m ()
termFromUI uiRes = (variantToValue <$> request ShutdownUI uiRes) >>= terminate 

--
showButtons :: Text -> [Text] -> ShowDyn
showButtons p txts = ShowDyn (mkBtnBar p txts)

showNotice :: Text -> ShowStatic
showNotice txt = ShowStatic (mkStaticBar txt)

--
class GetFormEntries st where
  getFormEntris :: Proxy st -> [FormEntry] --return entries that is needed to collect required data
  getFormEntris _ = []

class GetStCard st where
  getStCard :: Proxy st -> UICard

instance GetStCard' st (StateDataTrans st) => GetStCard st where 
  getStCard px_st = getStCard' px_st (Proxy @(StateDataTrans st))
  
class GetStCard' st (stTrans :: StateTransKind) where
  getStCard' :: Proxy st -> Proxy stTrans -> UICard

instance GetDynCard st => GetStCard' st 'Dynamic where
  getStCard' px_st _stTrans = getDynCard px_st

instance KnownSymbol (StateTitle st) => GetStCard' st 'Static where
  getStCard' _px_st _stTrans = mkStaticBar (pack $ symbolVal (Proxy @(StateTitle st))) 

class GetDynCard st where
  getDynCard :: Proxy st -> UICard

instance (StateDataTrans st ~ 'Dynamic, GetStatesForm (NextDataStates st)) => GetDynCard st where
  getDynCard _p_st = Form $ getStatesForm (Proxy @(NextDataStates st))     

class GetStatesForm (sts :: [*]) where
  getStatesForm :: Proxy sts ->  FormParams

instance GetStatesForm '[] where
  getStatesForm _px_sts = mempty

instance (GetFormEntries st , KnownSymbol (StateTitle st), GetStatesForm sts) => GetStatesForm (st ': sts) where
  getStatesForm _px_sts = 
    let stName :: Text
        stName = pack $ symbolVal (Proxy @(StateTitle st))
        entries = getFormEntris (Proxy @st)
        resps = respForEntries entries
        btn = [Button stName stName Info]
    in (FormParams entries resps btn) <> getStatesForm (Proxy @sts) 

data UIStData dynOrStatic = UIStData 
  { curUIState :: dynOrStatic
  , curUICard :: UICard 
  , prevUIResp :: UIResp
  }

type UIDynData m = UIStData (UIDyn m)
type UIStaticData m = UIStData (UIStatic m)

class HandleUIResp m st where
  handleUIResp :: UIDynData m -> StUIRespReceived m UI "ui" -> m st

instance HandleUIResp' m st (StateDataTrans st) => HandleUIResp m st where
  handleUIResp  = handleUIResp' (Proxy @(StateDataTrans st)) 
  
class (stTrans ~ StateDataTrans st) => HandleUIResp' m st (stTrans :: StateTransKind) where
  handleUIResp' :: Proxy stStrans -> UIDynData m -> StUIRespReceived m UI "ui" -> m st

instance (TaskPoster m, GetStCard st, Coercible (UIDynData m) st, StateDataTrans st ~ 'Dynamic) => HandleUIResp' m st 'Dynamic where
  handleUIResp' _stStrans (UIStData _ _uiCard _) uiRespRcvd = do
    uiRsp <- uiResp uiRespRcvd
    let uiCard = getStCard (Proxy @st)
    v_newUIState <- request (ShowDyn uiCard) uiRespRcvd
    let newUIState = variantToValue v_newUIState
    return $ coerce $ UIStData newUIState uiCard uiRsp

instance (TaskPoster m, GetStCard st, Coercible (UIStaticData m) st, StateDataTrans st ~ 'Static) => HandleUIResp' m st 'Static where
  handleUIResp' _stStrans (UIStData _ _uiCard _) uiRespRcvd = do
    uiRsp <- uiResp uiRespRcvd
    let uiCard = getStCard (Proxy @st)
    v_newUIState <- request (ShowStatic uiCard) uiRespRcvd
    let newUIState = variantToValue v_newUIState
    return $ coerce $ UIStData newUIState uiCard uiRsp

class HandleUIRespVar m sts where
  handleUIRespVar :: Int -> UIDynData m -> StUIRespReceived m UI "ui" -> m (V sts)

instance (TaskPoster m ,HandleUIResp m st) => HandleUIRespVar m '[st] where
  handleUIRespVar _index dd respReceived = variantFromValue <$> handleUIResp dd respReceived

instance 
  ( TaskPoster m 
  , HandleUIRespVar m '[st]
  , HandleUIRespVar m (st1 ': sts)
  , Liftable '[st] (st ': st1 ': sts)
  , Liftable (st1 ': sts) (st ': st1 ': sts)
  ) => HandleUIRespVar m (st ': st1 ': sts) where
  handleUIRespVar 0 dd respReceived = do 
    (v_st :: V '[st]) <- handleUIRespVar 0 dd respReceived
    return $ liftVariant v_st 
  handleUIRespVar indx dd respReceived = do 
    (v_st1sts :: V (st1 ': sts)) <- handleUIRespVar (indx - 1) dd respReceived
    return $ liftVariant v_st1sts 

transFromUIDynData :: forall m sts. 
  ( HandleUIRespVar m sts
  , TaskPoster m
  ) => UIDynData m -> (V sts -> m ()) -> m ()
transFromUIDynData uiDynData cbFunc = do
  let uiDynSt :: UIDyn m
      uiDynSt = curUIState uiDynData
  void $ next uiDynSt (\uiRespReceivedVar -> do
      let respReceived =  variantToValue uiRespReceivedVar
      uiRsp <- uiResp respReceived
      let  userChoiceIndex = uiRespIndex (curUICard uiDynData) uiRsp
      v_sts <- handleUIRespVar userChoiceIndex uiDynData respReceived    
      cbFunc v_sts
      return True)

uiTrans :: (HandleUIRespVar m sts, TaskPoster m, Coercible a (UIDynData m)) => a -> (V sts -> m ()) -> m ()
uiTrans = transFromUIDynData . coerce

newResFromUIData :: forall resParams m initUIReq initResState a.
  ( Coercible resParams UIParams
  , TaskPoster m 
  , GetStCard initResState
  , ShowReqForTrans (StateDataTrans initResState) initUIReq
  , Request m initUIReq (St (UIInitialized m UI) "ui")
  , ReqResult initUIReq (St (UIInitialized m UI) "ui") ~ '[a]
  , Coercible (UIStData a) initResState) => resParams -> m initResState
newResFromUIData resParams = do
  let uiCard = getStCard (Proxy @initResState)
  uiResPar <- uiResParams resParams
  uiInitilized <- fmap (stFromName #ui) (mkRes uiResPar)
  uiShowing <- request ((showReqForTrans (Proxy @(StateDataTrans initResState))) uiCard) uiInitilized
  return  $ coerce (UIStData (variantToValue uiShowing) uiCard emptyUIResp) 

class ShowReqForTrans (stTrans :: StateTransKind) showReq | stTrans -> showReq where
  showReqForTrans :: Proxy stTrans -> UICard -> showReq

instance ShowReqForTrans 'Static ShowStatic where
  showReqForTrans _ = ShowStatic

instance ShowReqForTrans 'Dynamic ShowDyn where
  showReqForTrans _ = ShowDyn

termUIData :: (TaskPoster m) => UIStaticData m -> m ()
termUIData uiState = (variantToValue <$> request ShutdownUI (curUIState uiState)) >>= terminate 

termUI :: (TaskPoster m, Coercible (UIStaticData m) st) => st -> m ()
termUI = termUIData . coerce

class ReqUI1 m curUISt st  where
  reqUI1 :: UIStData curUISt -> m (V '[st])

instance (ReqUI1' m curUISt st (StateDataTrans st)) => ReqUI1 m curUISt st where
  reqUI1 = reqUI1' (Proxy @(StateDataTrans st))

class ReqUI1' m curUISt st (nextUISt :: StateTransKind) where
  reqUI1' :: Proxy nextUISt -> UIStData curUISt -> m (V '[st])

instance (GetStCard st, Request m ShowDyn curUISt, Coercible (UIDynData m) st, ReqResult ShowDyn curUISt ~ '[UIDyn m]) => ReqUI1' m curUISt st 'Dynamic where
  reqUI1' _ curStData = do
    let card = getStCard (Proxy @st)
    nextUISt <- variantToValue <$> request (ShowDyn card) (curUIState curStData)
    return $ variantFromValue $ coerce (UIStData nextUISt card emptyUIResp)

instance (GetStCard st, Request m ShowStatic curUISt, Coercible (UIStaticData m) st, ReqResult ShowStatic curUISt ~ '[UIStatic m]) => ReqUI1' m curUISt st 'Static where
  reqUI1' _ curStData = do
    let card = getStCard (Proxy @st)
    nextUISt <- variantToValue <$> request (ShowStatic card) (curUIState curStData)
    return $ variantFromValue $ coerce (UIStData nextUISt card emptyUIResp)

{-  
reqUI1 :: (GetStCard st) => uiReq -> uiRes -> m (V '[st]) 
reqUI1 uiReq uiRes = do
  newUIState <- request uiReq uiRes
  return $ coerceVar newUIState 
-}


---
getTextFromResp :: UIStData st -> Text -> Text
getTextFromResp uiStData txt = getFormTextRes (prevUIResp uiStData) txt

getNumFromResp :: UIStData st -> Text -> Int
getNumFromResp uiStData txt = getFormNumRes (prevUIResp uiStData) txt 

getBoolFromResp :: UIStData st -> Text -> Bool
getBoolFromResp uiStData txt = getFormBoolRes (prevUIResp uiStData) txt 

{-
data NoticeBarD (msg :: Symbol)
data ButtonD (btnCaption :: Symbol)   
data ButtonsBarD (buttons :: [*]) 
-}
