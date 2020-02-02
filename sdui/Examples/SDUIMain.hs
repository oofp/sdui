{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude
import SDUI.Data.SDUIData
import Web.WebServer
import Web.WSServer
import            Pipes ((>->))
import qualified  Pipes.Concurrent as PC
import            Control.Concurrent.STM.TChan
import qualified  Pipes

newClientHandler :: NewClientCallback ClientResp ServerReq
newClientHandler (input, output) = 
  void $ async $ do
    putStrLn ("New connection detected"::Text) 
    recvChan <- newTChanIO
    _rcvTask <- async $ Pipes.runEffect $ PC.fromInput input >-> clientRespHandler recvChan   
    Pipes.runEffect $ serverReqProducer recvChan >-> PC.toOutput output

clientRespHandler :: TChan ClientResp -> Pipes.Consumer ClientResp IO ()
clientRespHandler chan = forever $ do  
  clientResp <- Pipes.await 
  liftIO $ atomically $ writeTChan chan clientResp   

serverReqProducer :: TChan ClientResp -> Pipes.Producer ServerReq IO ()
serverReqProducer chan = do
  let btnBar = ButtonBar $ ButtonBarParams "Bar title" [Button "btnCancel" "Cancel",Button "btnOK" "OK"]
  let createEntryReq = CreateEntry (EntryID "ID1") (EntryTitle "SDUIDemo") (ReqID 1) btnBar
  let btnBar2 = ButtonBar $ ButtonBarParams "Foor title" [Button "fooCancel" "FooCancel",Button "fooOK" "FooOK"]
  let createEntryReq2 = CreateEntry (EntryID "FooID1") (EntryTitle "SDUIDemoFoo") (ReqID 1) btnBar2
  Pipes.yield createEntryReq
  Pipes.yield createEntryReq2
  forever $ do
    (ClientResp entryID (ReqID reqID) _uiResp) <- liftIO $ atomically $ readTChan chan
    let barOrFoo = 
          case entryID of 
            (EntryID "FooID1") -> btnBar2
            _ -> btnBar 
    let newReq = ReplaceEntry entryID (ReqID $ reqID+1) barOrFoo
    Pipes.yield newReq


main :: IO ()
main = do
  wsApp <- getWSApp newClientHandler
  httpTask <- async $ runHttpApp "index.html" wsApp 8072

  putStrLn ("Press [Enter] to exit"::Text)
  void getLine
  cancel httpTask