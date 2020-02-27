{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module  UIApp where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on, Type)
import           Beseder.Base.ControlData                                               
import           Beseder.Base.Common
import           Beseder.Misc.Misc
import           Beseder.Resources.Timer
import           Data.String 
import           Beseder.SDUI.SDUIContext
import           SDUI.Data.SDUIData
import           Beseder.SDUI.SDUIRes
import           Beseder.SDUI.SDUIResImpl
import           Beseder.SDUI.SDUIHelper
import           GHC.Exts (Any)    

helloUiRes :: SDUIContext -> Beseder.SDUI.SDUIResImpl.ResPar TaskQ UI
helloUiRes ctx = MkUI (UIParams ctx (EntryID "ui1") (EntryTitle "Hello UI"))

uiApp :: SDUIContext -> STransData TaskQ NoSplitter _ () 
uiApp ctx = do        
  newRes #ui (helloUiRes ctx) -- (MkUI (UIParams ctx (EntryID "ui1") (EntryTitle "Hello UI")))
  --newRes #ui (MkUI (UIParams ctx (EntryID "ui1") (EntryTitle "Hello UI")))
  invoke #ui (showButtons "Hello Prompt" ["I am impressed","OK"])
  nextEv
  newRes #t1 TimerRes                    
  invoke #t1  (StartTimer 5)   
  nextEv                                
  clear #t1   
  invoke #ui (showButtons "Hello Prompt2" ["I am impressed again","OK"])
  nextEv
  invoke #ui (showNotice "Bye bye now")
  newRes #t1 TimerRes                    
  invoke #t1  (StartTimer 5)   
  nextEv                                 
  clear #t1
  invoke #ui ShutdownUI                              
  clear #ui                              

mkSTransDataTypeAny "uiApp" "UiApp"
type UiRes = Eval (UiApp NoSplitter '[()])
runUiApp ctx = runAsyncData $ uiApp ctx

uiRes :: Text -> SDUIContext -> Beseder.SDUI.SDUIResImpl.ResPar TaskQ UI
uiRes name ctx = MkUI (UIParams ctx (EntryID name) (EntryTitle name))


uiApp2 :: SDUIContext -> STransData TaskQ NoSplitter _ () 
uiApp2 ctx = while $ do        
  newRes #ui1 (uiRes "Screen 1" ctx) 
  newRes #ui2 (uiRes "Screen 2" ctx)
  newRes #t1 TimerRes                    
  invoke #t1  (StartTimer 20)   

  invoke #ui1 (showButtons "Please Click (1)" ["OK 1"])
  invoke #ui2 (showButtons "Please Click (2)" ["OK 2"])
  pumpEvents
  invoke #ui1 ShutdownUI                              
  invoke #ui2 ShutdownUI                              
  clear #ui1  
  clear #ui2  
  clear #t1
  liftIO $ putStrLn ("uiApp2 completed" :: Text)
  return True

mkSTransDataTypeAny "uiApp2" "UiApp2"
type UiRes2 = Eval (UiApp2 NoSplitter '[()])
runUiApp2 ctx = runAsyncData $ uiApp2 ctx
