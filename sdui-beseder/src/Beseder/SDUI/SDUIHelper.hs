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
{-# LANGUAGE NoMonomorphismRestriction #-}

module Beseder.SDUI.SDUIHelper 
  ( transFromUI
  , transFromUI1
  , transFromBtn2
  , newResFromUI
  , reqFromUI
  , termFromUI
  , showButtons
  , showNotice
  ) where

import           Protolude
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.SDUI.SDUIResImpl
import qualified Beseder.SDUI.SDUIRes as SDUIRes
import           Beseder.SDUI.SDUIRes
-- import           Beseder.SDUI.SDUIContext
import           SDUI.Data.SDUIData 
import           SDUI.Data.UIHelper
import           Haskus.Utils.Variant
import           Data.Coerce

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
---

type FromUIResp sts = CreateFrom UIResp sts  


transFromUIResp :: 
  ( FromUIResp (Var ys)
  , TaskPoster m
  ) => UICard -> (V ys -> m ()) -> m ()
transFromUIResp uiCard respToStates cbFunc =
  void $ next uiCard (\uiRespReceived -> do
      let index = uiRespIndex uiCard uiRespReceived
          v_ys = unVar $ createFrom (IndexedPar index uiRespReceived)
      cbFunc v_ys
      return True)

