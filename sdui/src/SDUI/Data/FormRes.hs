{-# LANGUAGE TemplateHaskell #-}

module SDUI.Data.FormRes where 

import Protolude

import Elm.Derive

data FormItemRes 
  = IntRes Int
  | BoolRes Bool
  | TextRes Text
  deriving (Show, Eq)

data FormEntryRes  = FormEntryRes {entryID :: Text, result :: FormItemRes} deriving (Show, Eq)

data FormRespParams = FormRespParams
  { buttonClicked :: Text
  , entriesRes :: [FormEntryRes] 
  } deriving (Show, Eq)
  
deriveBoth defaultOptions ''FormItemRes
deriveBoth defaultOptions ''FormEntryRes
deriveBoth defaultOptions ''FormRespParams
