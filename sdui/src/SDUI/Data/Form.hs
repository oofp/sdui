{-# LANGUAGE TemplateHaskell #-}

module SDUI.Data.Form where 

import Protolude

import Elm.Derive
import SDUI.Data.Button
import SDUI.Data.FormRes

data InputType 
  = Text
  | Password 
  | DateTimeLocal
  | Date
  | Month
  | Time
  | Week
  | Number
  | Email
  | Url
  | Search
  | Tel
  | Color
  deriving (Show, Eq)

data SelectItem = SelectItem 
  { selectItemID :: Text 
  , selectItemText :: Text
  } deriving (Show, Eq)

data FormGroupItem 
  = Input InputType 
  | Select [SelectItem]  
  | TextArea Int 
  deriving (Show, Eq)

data FormGroupParams = FormGroupParams
  { label :: Maybe Text
  , item :: FormGroupItem 
  , help :: Maybe Text
  } deriving (Show, Eq)

data CheckBoxParams = CheckBoxParams Text deriving (Show, Eq)

data RadioItem = RadioItem 
  { radioItemText :: Text
  , radioItemID :: Text
  } deriving (Show, Eq)

data RadioListParams =  RadioListParams
  { legend :: Text
  , radioItems :: [RadioItem]
  } deriving (Show, Eq)

data FormItem 
  = FormGroup FormGroupParams  
  | CheckBox CheckBoxParams  
  | RadioList  RadioListParams 
  deriving (Show, Eq)

data FormEntry = FormEntry {entryID :: Text, formItem :: FormItem} deriving (Show, Eq)

data FormParams = FormParams
  { formEntries :: [FormEntry]
  , formEntriesResps :: [FormEntryRes] 
  , formButtons :: [Button]
  , formTitle :: Maybe Text
  } deriving (Show, Eq)

instance Semigroup FormParams where
  f1 <> f2 = FormParams (formEntries f1 <> formEntries f2) 
                        (formEntriesResps f1 <> formEntriesResps f2)
                        (formButtons f1 <> formButtons f2)
                        (formTitle f1 <|> formTitle f2)

instance Monoid FormParams where
  mempty = FormParams mempty mempty mempty Nothing

respForItem :: FormItem -> FormItemRes
respForItem (RadioList _) = TextRes ""
respForItem (CheckBox _) = BoolRes False
respForItem (FormGroup (FormGroupParams _ (TextArea _) _)) = TextRes "" 
respForItem (FormGroup (FormGroupParams _ (Select _) _)) = TextRes ""
respForItem (FormGroup (FormGroupParams _ (Input Number) _)) = IntRes 0
respForItem (FormGroup (FormGroupParams _ (Input _) _)) = TextRes ""

respForEntries :: [FormEntry] -> [FormEntryRes]
respForEntries = fmap (\(FormEntry enID item) -> FormEntryRes enID (respForItem item))

deriveBoth defaultOptions ''InputType
deriveBoth defaultOptions ''SelectItem
deriveBoth defaultOptions ''FormGroupItem
deriveBoth defaultOptions ''FormGroupParams
deriveBoth defaultOptions ''CheckBoxParams
deriveBoth defaultOptions ''RadioItem
deriveBoth defaultOptions ''RadioListParams
deriveBoth defaultOptions ''FormItem
deriveBoth defaultOptions ''FormEntry
deriveBoth defaultOptions ''FormParams


