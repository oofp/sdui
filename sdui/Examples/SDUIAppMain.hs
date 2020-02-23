{-# LANGUAGE OverloadedStrings #-}

module SDUIAppMain where

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
import            SDUIApp

runHello :: IO ()
runHello = do
  runServer 8072 "Hello App" $ forever $ do
    firstName <- askUI "First name:"
    lastName <- askUI "Last name:"
    showMsg $ "Hello, " <> firstName <> " " <> lastName


