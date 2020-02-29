{-# LANGUAGE OverloadedStrings #-}

module SDUIApp 
    ( SDUIApp
    , uiReq
    , uiReq_
    , runServer
    , askUI
    , showMsg
    ) where

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
import qualified  Pipes

type SDUIApp a = StateT (Text, Int) (Pipes.Pipe ClientResp ServerReq IO) a

uiReq :: UICard -> SDUIApp UIResp 
uiReq uiCard = do 
    uiReq_ uiCard
    (ClientResp _ _ uiResp) <- lift $ Pipes.await
    return uiResp        

uiReq_ :: UICard -> SDUIApp ()
uiReq_ uiCard = do 
    (appName, reqNum) <- get
    put (appName, reqNum+1)
    let srvReq = if (reqNum == 0)
            then CreateEntry (EntryID "ui") (EntryTitle appName) (ReqID reqNum) uiCard
            else ReplaceEntry (EntryID "ui") (ReqID reqNum) uiCard
    lift $ Pipes.yield srvReq 

newClientHandler :: Text -> SDUIApp () -> NewClientCallback ClientResp ServerReq
newClientHandler appTitle sduiApp (input, output) = 
  void $ async $ do
    putStrLn ("New connection detected"::Text) 
    Pipes.runEffect $ PC.fromInput input >-> evalStateT sduiApp (appTitle,0) >-> PC.toOutput output


runServer :: Int -> Text -> SDUIApp () -> IO () 
runServer port appTitle sduiApp = do
    wsApp <- getWSApp (newClientHandler appTitle sduiApp)
    httpTask <- async $ runHttpApp "index.html" wsApp port
    putStrLn ("Press [Enter] to exit"::Text)
    void getLine
    cancel httpTask
 
 --
askUI :: Text -> SDUIApp Text
askUI prompt = do 
    let entries = [ FormEntry "inpText" (FormGroup (FormGroupParams (Just prompt) (Input Form.Text) Nothing))]
        resps = respForEntries entries
        btns = [Button "Submit" "Submit"  Style.Success]            
        uiCard = Form $ FormParams entries resps btns Nothing 
    rsp <- uiReq uiCard
    case rsp of
        FormResp (FormRespParams _ [FormEntryRes _ (TextRes txt)]) -> return txt
        _ -> return ""


showMsg :: Text -> SDUIApp ()
showMsg msg = do 
    let uiCard = ButtonBar $ ButtonBarParams msg [Button "ok" "OK" Style.Info]
    void $ uiReq uiCard

