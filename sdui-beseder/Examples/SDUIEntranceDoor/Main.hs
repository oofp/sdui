{-# LANGUAGE OverloadedStrings #-}

module Main where

import            Protolude    
import            Beseder.SDUI.Env.SDUIEnv
import            EntranceDoor

main :: IO ()
main = startHttpApp runUiDoor "index.html" 8072 
