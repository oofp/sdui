{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module  UIAppTest where

import           Protolude                    hiding (Type)
import           Data.String 

import           Language.Haskell.TH

reifyValue :: String -> Q (Maybe Info) -- Q [Dec]
reifyValue valName = do
  maybeName <- lookupValueName valName
  case maybeName of 
    Nothing -> do
      liftIO $ putStrLn ("=================== Value NOT found"::Text)
      return Nothing -- [] -- Nothing
    (Just valName) -> do
      rInfo <- reify valName
      return $ Just rInfo
      --liftIO $ putStrLn ("=================== Value found"::Text)
      --return []

-- $(stringE . show =<< reifyValue "timer1")

getFuncType :: Info -> Maybe Type
--getFuncType (VarI _varName (ForallT _varBnd _ctx (AppT (AppT _ fType) _)) _maybeDec) = Just fType
getFuncType (VarI _varName (ForallT _varBnd _ctx nextType) _maybeDec) = getFuncType' nextType
getFuncType (VarI _varName nextType _maybeDec) = getFuncType' nextType
getFuncType _ = Nothing

getFuncType' :: Type -> Maybe Type
getFuncType' (AppT (AppT ArrowT _) nextType) = getFuncType' nextType
getFuncType' (AppT (AppT _ fType) _) = Just fType
getFuncType' _ = Nothing


-- VarI UIApp.uiApp (AppT (AppT ArrowT (ConT Beseder.SDUI.SDUIContext.SDUIContext)) (AppT (AppT (AppT (AppT (ConT Beseder.Base.Internal.STransData.STransData) (ConT Beseder.Misc.TaskPosterImpl.TaskQ.TaskQ)) (ConT Beseder.Base.Internal.SplitOps.NoSplitter)) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.ComposeFunc) (AppT (AppT (AppT (ConT Beseder.Base.Internal.STransDef.NewResFunc) (AppT (AppT (ConT Beseder.SDUI.SDUIRes.ResPar) (ConT Beseder.Misc.TaskPosterImpl.TaskQ.TaskQ)) (ConT Beseder.SDUI.SDUIResImpl.UI))) (LitT (StrTyLit "ui"))) (ConT Beseder.Misc.TaskPosterImpl.TaskQ.TaskQ))) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.ComposeFunc) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.InvokeAllFunc) (ConT Beseder.SDUI.SDUIRes.ShowDyn)) (LitT (StrTyLit "ui")))) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.ComposeFunc) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.CaptureFunc) (ConT Beseder.Base.Internal.SplitOps.Dynamics)) (ConT Beseder.Base.Internal.STransDef.GetNextAllFunc))) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.ComposeFunc) (AppT (AppT (AppT (ConT Beseder.Base.Internal.STransDef.NewResFunc) (ConT Beseder.Resources.Timer.TimerRes.TimerRes)) (LitT (StrTyLit "t1"))) (ConT Beseder.Misc.TaskPosterImpl.TaskQ.TaskQ))) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.ComposeFunc) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.InvokeAllFunc) (ConT Beseder.Resources.Timer.TimerRes.StartTimer)) (LitT (StrTyLit "t1")))) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.ComposeFunc) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.CaptureFunc) (ConT Beseder.Base.Internal.SplitOps.Dynamics)) (ConT Beseder.Base.Internal.STransDef.GetNextAllFunc))) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.ComposeFunc) (AppT (ConT Beseder.Base.Internal.STransDef.ClearAllFunc) (LitT (StrTyLit "t1")))) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.ComposeFunc) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.InvokeAllFunc) (ConT Beseder.SDUI.SDUIRes.ShowDyn)) (LitT (StrTyLit "ui")))) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.ComposeFunc) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.CaptureFunc) (ConT Beseder.Base.Internal.SplitOps.Dynamics)) (ConT Beseder.Base.Internal.STransDef.GetNextAllFunc))) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.ComposeFunc) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.InvokeAllFunc) (ConT Beseder.SDUI.SDUIRes.ShowStatic)) (LitT (StrTyLit "ui")))) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.ComposeFunc) (AppT (AppT (AppT (ConT Beseder.Base.Internal.STransDef.NewResFunc) (ConT Beseder.Resources.Timer.TimerRes.TimerRes)) (LitT (StrTyLit "t1"))) (ConT Beseder.Misc.TaskPosterImpl.TaskQ.TaskQ))) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.ComposeFunc) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.InvokeAllFunc) (ConT Beseder.Resources.Timer.TimerRes.StartTimer)) (LitT (StrTyLit "t1")))) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.ComposeFunc) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.CaptureFunc) (ConT Beseder.Base.Internal.SplitOps.Dynamics)) (ConT Beseder.Base.Internal.STransDef.GetNextAllFunc))) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.ComposeFunc) (AppT (ConT Beseder.Base.Internal.STransDef.ClearAllFunc) (LitT (StrTyLit "t1")))) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.ComposeFunc) (AppT (AppT (ConT Beseder.Base.Internal.STransDef.InvokeAllFunc) (ConT Beseder.SDUI.SDUIRes.ShutdownUI)) (LitT (StrTyLit "ui")))) (AppT (ConT Beseder.Base.Internal.STransDef.ClearAllFunc) (LitT (StrTyLit "ui"))))))))))))))))))) (TupleT 0))) Nothing

reifyFunc :: String -> Q (Maybe Type)
reifyFunc valName = do
  maybeInfo <- reifyValue valName
  return (maybeInfo >>= getFuncType) 

-- $(stringE . show =<< reifyFunc "uiApp")