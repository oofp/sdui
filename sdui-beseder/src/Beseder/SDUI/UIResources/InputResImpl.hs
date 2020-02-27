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

module Beseder.SDUI.UIResources.InputResImpl where

import           Protolude    
import           Beseder.Base.Common
import           Beseder.SDUI.SDUIResImpl
import           SDUI.Data.Form  
import           Beseder.SDUI.SDUIHelper  
import           Beseder.SDUI.UIResources.InputRes

data UIInp = UIInp deriving Show

instance TaskPoster m => InputRes m UIInp where
  newtype  Inputing m UIInp = Inputing (UIDynData m)  
  newtype  CollectedText m UIInp = CollectedText (UIDynData m)
  newtype  CollectedNum m UIInp = CollectedNum (UIDynData m)
  newtype  CollectedBool m UIInp = CollectedBool (UIDynData m)
  newtype  InputCompleted m UIInp = InputCompleted (UIStaticData m) 
  newtype  InputStopped m UIInp = InputStopped (UIStaticData m) 
  newtype ResPar m UIInp = MkUIInp UIParams 

  initUI = newResFromUIData
  inputingTransition  = uiTrans
  textTransition = uiTrans
  numTransition = uiTrans
  boolTransition = uiTrans 
  termCompleted = termUI
  termStopped = termUI

  stopInputting _ (Inputing st) = reqUI1 st
  stopText _ (CollectedText st) = reqUI1 st
  stopNum _ (CollectedNum st) = reqUI1 st
  stopBool _ (CollectedBool st) = reqUI1 st

  _collectedText (CollectedText st) = return $ getTextFromResp st "inputText" 
  _collectedNum (CollectedNum st) = return $ getNumFromResp st "inputNum" 
  _collectedBool (CollectedBool st) = return $ getBoolFromResp st "inputBool"

instance GetFormEntries (CollectedText m UIInp) where
  getFormEntris _ = [FormEntry "inputText" (FormGroup (FormGroupParams (Just "Enter some text") (Input Text) (Just "just any text, please")))]

instance GetFormEntries (CollectedNum m UIInp) where
  getFormEntris _ = [FormEntry "inputNum" (FormGroup (FormGroupParams (Just "Enter some number") (Input Number) (Just "just any number, please")))]

instance GetFormEntries (CollectedBool m UIInp) where
  getFormEntris _ = [FormEntry "inputBool" (CheckBox (CheckBoxParams "Yes or not"))]

instance GetFormEntries (Inputing m UIInp) where
  getFormEntris _ = []

instance GetFormEntries (InputCompleted m UIInp) where
  getFormEntris _ = []

instance GetFormEntries (InputStopped m UIInp) where
  getFormEntris _ = []
