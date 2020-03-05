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
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels       #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Beseder.SDUI.SDUIResHelper 
  ( uiInteract
  , getOptionVar
  , uiUnit
  , uiBool
  , uiShowText
  ) where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                              (>>), (>>=), forever, until,try,on)
import           Beseder.Base.Common
import           Beseder.Base.ControlData
import           Beseder.SDUI.SDUIResImpl
import           Beseder.SDUI.SDUIRes
import           SDUI.Data.SDUIData 
--import           SDUI.Data.Style
import           SDUI.Data.UIHelper
import           Haskus.Utils.Variant
import           Beseder.Utils

uiInteract :: forall uiResName m sp. (TaskPoster m) =>  Named uiResName -> UICard -> STransData m sp _ UIResp
uiInteract uiName uiCard = do
  invoke uiName (ShowDyn uiCard)
  skipTo @(uiResName :? IsUIRespReceived)
  opRes uiName uiResp'  


getOptionVar :: forall uiResName m sp options. (TaskPoster m, ListNames options, GetVarInstance options) => Named uiResName -> Text -> Proxy options -> STransData m sp _ (V options)
getOptionVar uiName prompt px = do
  let btnBar = mkBtnBar prompt (listNames px)
  rsp <- uiInteract uiName btnBar
  let respIndex = uiRespIndex btnBar rsp
  return $ getVarInstance respIndex

uiUnit :: forall uiResName m sp. (TaskPoster m) => Named uiResName -> Text -> Text -> STransData m sp _ ()
uiUnit uiName prompt ackButton = do
  let btnBar = mkBtnBar prompt [ackButton]
  _rsp <- uiInteract uiName btnBar
  return ()

uiBool :: forall uiResName m sp. (TaskPoster m) => Named uiResName -> Text -> Text -> Text -> STransData m sp _ Bool
uiBool uiName prompt yesButton noButton = do
  let btnBar = mkBtnBar prompt [yesButton,noButton]
  rsp <- uiInteract uiName btnBar
  let respIndex = uiRespIndex btnBar rsp
  return (respIndex == 0)

uiShowText :: forall uiResName m sp. (TaskPoster m) => Named uiResName -> Text -> STransData m sp _ ()
uiShowText uiName txt = invoke uiName (ShowStatic $ mkStaticBar txt)