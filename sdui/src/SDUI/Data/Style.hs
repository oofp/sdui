{-# LANGUAGE TemplateHaskell #-}

module SDUI.Data.Style where 

import Protolude

import Elm.Derive

data Style 
    = Primary
    | Secondary
    | Success
    | Info
    | Warning
    | Danger
    | Light
    | Dark
    | Link  
    deriving (Show, Eq)

deriveBoth defaultOptions ''Style

