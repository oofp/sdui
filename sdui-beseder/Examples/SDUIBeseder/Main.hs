{-# LANGUAGE OverloadedStrings #-}

module Main where

import            Protolude    
import            Beseder.SDUI.Env.SDUIEnv
import            UIApp

main :: IO ()
main = startHttpApp runUiApp2 "index.html" 8072 
