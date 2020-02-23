{-# LANGUAGE OverloadedStrings #-}

module SDUIAppMain where

import            Protolude
import            SDUIApp

runHello :: IO ()
runHello = do
  runServer 8072 "Hello App" $ forever $ do
    firstName <- askUI "First name:"
    lastName <- askUI "Last name:"
    showMsg $ "Hello, " <> firstName <> " " <> lastName


