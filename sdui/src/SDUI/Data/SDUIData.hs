{-# LANGUAGE TemplateHaskell #-}

module SDUI.Data.SDUIData where 

import Protolude
import SDUI.Data.Style
import SDUI.Data.Button
import SDUI.Data.Form
import SDUI.Data.FormRes

import Elm.Derive
import Elm.Module

data ButtonBarParams = ButtonBarParams
  { bbPrompt :: Text
  , bbButtons :: [Button]
  } deriving (Show, Eq)

deriveBoth defaultOptions ''ButtonBarParams

data StaticBarParams = StaticBarParams
  { staticBarNotice :: Text
  } deriving (Show, Eq)
deriveBoth defaultOptions ''StaticBarParams

data UICard 
  = ButtonBar ButtonBarParams
  | StaticBar StaticBarParams
  | Form FormParams
  deriving (Show, Eq)

deriveBoth defaultOptions ''UICard

data UIResp 
  = ButtonClickedResp Text -- return btnID
  | FormResp FormRespParams -- return btnID
  deriving (Show, Eq)

emptyUIResp :: UIResp
emptyUIResp = FormResp (FormRespParams "" [])

deriveBoth defaultOptions ''UIResp

newtype ReqID = ReqID Int deriving (Show, Eq)
newtype EntryID = EntryID Text deriving (Show, Eq)
newtype EntryTitle = EntryTitle Text deriving (Show, Eq)
newtype SessionID = SessionID Int deriving (Show, Eq)
deriveBoth defaultOptions ''ReqID
deriveBoth defaultOptions ''EntryID
deriveBoth defaultOptions ''EntryTitle
deriveBoth defaultOptions ''SessionID

data ServerReq 
  = CreateEntry EntryID EntryTitle ReqID UICard 
  | DeleteEntry EntryID 
  | ReplaceEntry EntryID ReqID UICard
  deriving (Show, Eq)

deriveBoth defaultOptions ''ServerReq

data ClientResp = ClientResp EntryID ReqID UIResp deriving (Show, Eq)

deriveBoth defaultOptions ''ClientResp

printElm :: IO ()
printElm =
    putStrLn $ makeElmModule "SDUI"
    [ DefineElm (Proxy :: Proxy EntryID)
    , DefineElm (Proxy :: Proxy ReqID)
    , DefineElm (Proxy :: Proxy EntryTitle)
    , DefineElm (Proxy :: Proxy SessionID)
    , DefineElm (Proxy :: Proxy Style)
    , DefineElm (Proxy :: Proxy InputType)
    , DefineElm (Proxy :: Proxy SelectItem)
    , DefineElm (Proxy :: Proxy FormGroupItem)
    , DefineElm (Proxy :: Proxy FormGroupParams)
    , DefineElm (Proxy :: Proxy CheckBoxParams)
    , DefineElm (Proxy :: Proxy RadioItem)
    , DefineElm (Proxy :: Proxy RadioListParams)
    , DefineElm (Proxy :: Proxy FormEntry)
    , DefineElm (Proxy :: Proxy FormItem)
    , DefineElm (Proxy :: Proxy FormParams)
    , DefineElm (Proxy :: Proxy FormItemRes)
    , DefineElm (Proxy :: Proxy FormEntryRes)
    , DefineElm (Proxy :: Proxy FormRespParams)
    , DefineElm (Proxy :: Proxy Button)
    , DefineElm (Proxy :: Proxy ButtonBarParams)
    , DefineElm (Proxy :: Proxy StaticBarParams)
    , DefineElm (Proxy :: Proxy UICard)
    , DefineElm (Proxy :: Proxy UIResp)
    , DefineElm (Proxy :: Proxy ServerReq)
    , DefineElm (Proxy :: Proxy ClientResp)
    ]
