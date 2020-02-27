{-# LANGUAGE OverloadedStrings #-}

module Main where

import            Protolude    
import            Beseder.SDUI.SDUIContext
import            SDUI.Data.SDUIData
import            Beseder.SDUI.Env.SDUIEnv
import            EntranceDoor

main :: IO ()
main = runUiDoor startHttpApp "index.html" 8072 
