{-# LANGUAGE TemplateHaskell #-}

module SDUI.Data.Button where 

import Protolude
import SDUI.Data.Style

import Elm.Derive

data Button = Button
  { btnId :: Text
  , btnCaption :: Text
  , btnStyle :: Style
  } deriving (Show, Eq)

deriveBoth defaultOptions ''Button

