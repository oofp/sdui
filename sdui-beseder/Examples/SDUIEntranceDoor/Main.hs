{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude    
import Beseder.SDUI.SDUIContext
import SDUI.Data.SDUIData
import Web.WebServer
import Web.WSServer
import            Pipes ((>->))
import qualified  Pipes.Concurrent as PC
import qualified  Pipes
import            Control.Concurrent.STM.TChan
import            EntranceDoor

newClientHandler :: (SDUIContext -> IO ()) -> NewClientCallback ClientResp ServerReq
newClientHandler appTask (input, output) = void $ async $ do
  putStrLn ("New connection detected"::Text)
  let sendFunc :: ServerReq -> IO ()
      sendFunc serverReq = do
        -- liftIO $ putStrLn ("Sending server req"::Text)
        Pipes.runEffect $ (Pipes.yield serverReq)  >-> PC.toOutput output
  clientRespDistributer <- initSDUIProc appTask sendFunc 
  putStrLn ("Starting client listener"::Text)
  _ <- async $ Pipes.runEffect $ PC.fromInput input >-> clientRespHandler clientRespDistributer 
  putStrLn ("Client listener started"::Text)

clientRespHandler :: (ClientResp -> IO ()) -> Pipes.Consumer ClientResp IO ()
clientRespHandler respDistributer = forever $ do  
  -- liftIO $ putStrLn ("Client listener awaiting"::Text)
  clientResp <- Pipes.await 
  liftIO $ respDistributer clientResp


main :: IO ()
main = do
  wsApp <- getWSApp (newClientHandler appLauncher)
  httpTask <- async $ runHttpApp "index.html" wsApp 8072

  putStrLn ("Press [Enter] to exit"::Text)
  void getLine
  cancel httpTask

appLauncher :: SDUIContext -> IO () 
appLauncher sduiCtx = runUiDoor sduiCtx

---
--main :: IO ()
--main = do
--    putStrLn ("Visit http://localhost:8000/ for try the app" :: Text)

