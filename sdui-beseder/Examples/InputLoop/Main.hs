{-# LANGUAGE OverloadedStrings #-}

module Main where

import            Protolude    
import            Beseder.SDUI.Env.SDUIEnv
--import qualified  InputLoopApp
import qualified  InputLoopTimerApp

main :: IO ()
main = startHttpApp InputLoopTimerApp.runInputs "index.html" 8072 

