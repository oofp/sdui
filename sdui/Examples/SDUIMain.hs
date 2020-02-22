{-# LANGUAGE OverloadedStrings #-}

module Main where

import            Protolude
import            SDUI.Data.SDUIData
import            SDUI.Data.Button
import            SDUI.Data.Form
import            SDUI.Data.FormRes
import qualified  SDUI.Data.Form as Form
import qualified  SDUI.Data.Style as Style
import            Web.WebServer
import            Web.WSServer
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
  let btnBar = ButtonBar $ ButtonBarParams "Bar title" [Button "btnCancel" "Cancel" Style.Warning,Button "btnOK" "OK" Style.Success]
  let createEntryReq = CreateEntry (EntryID "ID1") (EntryTitle "SDUIDemo") (ReqID 1) btnBar
  let btnBar2 = ButtonBar $ ButtonBarParams "Foor title" [Button "fooCancel" "FooCancel" Style.Danger,Button "fooOK" "FooOK" Style.Info]
  let createEntryReq2 = CreateEntry (EntryID "FooID1") (EntryTitle "SDUIDemoFoo") (ReqID 1) btnBar2
  let btns = [Button "Submit" "Submit" Style.Success, Button "Cancel" "Cancel" Style.Secondary]
      entries = [ FormEntry "inpText" (FormGroup (FormGroupParams (Just "First name:") (Input Form.Text) (Just "like Vanja")))
                , FormEntry "inpNumber" (FormGroup (FormGroupParams (Just "Age:") (Input Form.Number) (Just "like 42")))
                , FormEntry "inpDate" (FormGroup (FormGroupParams (Just "Date:") (Input Form.Date) (Just "some date")))
                ]
      resps = respForEntries entries            
      form = Form $ FormParams entries resps btns
      createFormEntry = CreateEntry (EntryID "form") (EntryTitle "First form") (ReqID 1) form 
  Pipes.yield createEntryReq
  Pipes.yield createEntryReq2
  Pipes.yield createFormEntry
  forever $ do
    (ClientResp entryID (ReqID reqID) uiResp) <- liftIO $ atomically $ readTChan chan
    let nextCard = 
          case (entryID, uiResp) of 
            (EntryID "FooID1",_) -> btnBar2
            (EntryID "FooID1",_) -> btnBar 
            (EntryID "form", FormResp formRespParams) -> Form $ FormParams entries (entriesRes formRespParams) btns
    let newReq = ReplaceEntry entryID (ReqID $ reqID+1) nextCard
    Pipes.yield newReq

main :: IO ()
main = do
  wsApp <- getWSApp newClientHandler
  httpTask <- async $ runHttpApp "index.html" wsApp 8072

  putStrLn ("Press [Enter] to exit"::Text)
  void getLine
  cancel httpTask