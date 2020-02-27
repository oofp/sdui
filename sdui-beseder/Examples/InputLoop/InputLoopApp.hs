{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell        #-}

{-# OPTIONS_GHC -fomit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module  InputLoopApp where

import           Protolude                    hiding (Product, Any, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until,try,on)
import           Beseder.Base.ControlData
import           Beseder.SDUI.UIResources.InputRes
import           Beseder.SDUI.UIResources.InputResImpl
import           Data.String 
import           Beseder.Misc.Misc
import           Beseder.SDUI.SDUIContext
import           SDUI.Data.SDUIData
import           Beseder.SDUI.SDUIResImpl
import           GHC.Exts (Any)    

inpRes :: SDUIContext -> Beseder.SDUI.UIResources.InputRes.ResPar TaskQ UIInp
inpRes ctx = MkUIInp (UIParams ctx (EntryID "inps") (EntryTitle "Inputs"))

inputsApp :: SDUIContext -> STransData TaskQ NoSplitter _ ()
inputsApp ctx = do
  newRes #inps (inpRes ctx)
  handleEvents $ do
    on @("inps" :? IsCollectedText) $ do
      txt <- opRes #inps collectedText  
      liftIO $ putStrLn ("text collected: " <> txt)
    on @("inps" :? IsCollectedNum) $ do
      num <- opRes #inps collectedNum  
      liftIO $ putStrLn (("num collected: " :: Text) <> show num)
    on @("inps" :? IsCollectedBool) $ do
      fl <- opRes #inps collectedBool  
      liftIO $ putStrLn (("num collected: " :: Text) <> show fl)
  clear #inps
  liftIO $ putStrLn ("inputsApp completed"::Text)

mkSTransDataTypeAny "inputsApp" "InputHandler"
-- :kind!  ValidateSteps '[] InputHandler NoSplitter '[()]
-- :kind! StateDiagramSym  InputHandler '[()]

runInputs :: SDUIContext -> IO ()
runInputs ctx = runAsyncData $ inputsApp ctx


