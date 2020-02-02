{-# LANGUAGE TemplateHaskell #-}

module SDUI.Data.SDUIData where 

import Protolude

import Elm.Derive
import Elm.Module

data Button = Button
  { btnId :: Text
  , btnCaption :: Text
  } deriving (Show, Eq)

deriveBoth defaultOptions ''Button

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
  deriving (Show, Eq)

deriveBoth defaultOptions ''UICard

data UIResp 
  = ButtonClickedResp Text -- return btnID
  deriving (Show, Eq)

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
    , DefineElm (Proxy :: Proxy Button)
    , DefineElm (Proxy :: Proxy ButtonBarParams)
    , DefineElm (Proxy :: Proxy StaticBarParams)
    , DefineElm (Proxy :: Proxy UICard)
    , DefineElm (Proxy :: Proxy UIResp)
    , DefineElm (Proxy :: Proxy ServerReq)
    , DefineElm (Proxy :: Proxy ClientResp)
    ]
