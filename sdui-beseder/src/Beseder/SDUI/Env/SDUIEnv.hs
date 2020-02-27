{-# LANGUAGE OverloadedStrings #-}

module Beseder.SDUI.Env.SDUIEnv
    ( startHttpApp 
    ) where

import            Protolude    
import            Beseder.SDUI.SDUIContext
import            SDUI.Data.SDUIData
import            Web.WebServer
import            Web.WSServer
import            Pipes ((>->))
import qualified  Pipes.Concurrent as PC
import qualified  Pipes
import            Data.Text (unpack)

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


startHttpApp :: (SDUIContext -> IO ()) -> Text -> Int -> IO ()
startHttpApp sduiApp initHTMLPage port = do
  wsApp <- getWSApp (newClientHandler sduiApp)
  httpTask <- async $ runHttpApp (unpack initHTMLPage) wsApp port

  putStrLn ("Visit http://localhost:" <> show port <> " to try the app"::Text)
  putStrLn ("Press [Enter] to exit"::Text)
  void getLine
  cancel httpTask

