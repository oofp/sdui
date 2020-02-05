
module Beseder.SDUI.SDUIContext where

import Protolude
import SDUI.Data.SDUIData  
import qualified StmContainers.Map as M

-- allocated per client connection
data SDUIContext = SDUIContext 
  { respListeners :: M.Map Text (ClientResp -> IO ())
  , senderFunc :: ServerReq -> IO ()      
  }

-- this method called to dis
clientHandler :: SDUIContext -> ClientResp -> IO ()
clientHandler ctx resp@(ClientResp (EntryID entryID) _ _) = do
  lstMaybe <- atomically $ M.lookup (entryID) (respListeners ctx)
  forM_ lstMaybe (\lst -> lst resp)

initSDUIProc :: (SDUIContext -> IO ()) -> (ServerReq -> IO ()) -> IO (ClientResp -> IO ())
initSDUIProc sduiProc serverReqSender = do
    listeners <- M.newIO
    let ctx = SDUIContext listeners serverReqSender
        clnHandler = clientHandler ctx 
    void $ async $ sduiProc ctx
    return clnHandler    


setListener :: SDUIContext -> EntryID -> (ClientResp -> IO ()) -> IO ()
setListener ctx (EntryID entryID) l = 
  atomically $ M.insert l entryID (respListeners ctx) 

removeListener :: SDUIContext -> EntryID -> IO ()
removeListener ctx (EntryID entryID) = 
  atomically $ M.delete entryID (respListeners ctx)   