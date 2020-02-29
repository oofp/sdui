module SDUI exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)


type alias EntryID  = String

jsonDecEntryID : Json.Decode.Decoder ( EntryID )
jsonDecEntryID =
    Json.Decode.string

jsonEncEntryID : EntryID -> Value
jsonEncEntryID  val = Json.Encode.string val



type alias ReqID  = Int

jsonDecReqID : Json.Decode.Decoder ( ReqID )
jsonDecReqID =
    Json.Decode.int

jsonEncReqID : ReqID -> Value
jsonEncReqID  val = Json.Encode.int val



type alias EntryTitle  = String

jsonDecEntryTitle : Json.Decode.Decoder ( EntryTitle )
jsonDecEntryTitle =
    Json.Decode.string

jsonEncEntryTitle : EntryTitle -> Value
jsonEncEntryTitle  val = Json.Encode.string val



type alias SessionID  = Int

jsonDecSessionID : Json.Decode.Decoder ( SessionID )
jsonDecSessionID =
    Json.Decode.int

jsonEncSessionID : SessionID -> Value
jsonEncSessionID  val = Json.Encode.int val



type Style  =
    Primary
    | Secondary
    | Success
    | Info
    | Warning
    | Danger
    | Light
    | Dark
    | Link

jsonDecStyle : Json.Decode.Decoder ( Style )
jsonDecStyle =
    let jsonDecDictStyle = Dict.fromList [("Primary", Primary), ("Secondary", Secondary), ("Success", Success), ("Info", Info), ("Warning", Warning), ("Danger", Danger), ("Light", Light), ("Dark", Dark), ("Link", Link)]
    in  decodeSumUnaries "Style" jsonDecDictStyle

jsonEncStyle : Style -> Value
jsonEncStyle  val =
    case val of
        Primary -> Json.Encode.string "Primary"
        Secondary -> Json.Encode.string "Secondary"
        Success -> Json.Encode.string "Success"
        Info -> Json.Encode.string "Info"
        Warning -> Json.Encode.string "Warning"
        Danger -> Json.Encode.string "Danger"
        Light -> Json.Encode.string "Light"
        Dark -> Json.Encode.string "Dark"
        Link -> Json.Encode.string "Link"



type InputType  =
    Text
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

jsonDecInputType : Json.Decode.Decoder ( InputType )
jsonDecInputType =
    let jsonDecDictInputType = Dict.fromList [("Text", Text), ("Password", Password), ("DateTimeLocal", DateTimeLocal), ("Date", Date), ("Month", Month), ("Time", Time), ("Week", Week), ("Number", Number), ("Email", Email), ("Url", Url), ("Search", Search), ("Tel", Tel), ("Color", Color)]
    in  decodeSumUnaries "InputType" jsonDecDictInputType

jsonEncInputType : InputType -> Value
jsonEncInputType  val =
    case val of
        Text -> Json.Encode.string "Text"
        Password -> Json.Encode.string "Password"
        DateTimeLocal -> Json.Encode.string "DateTimeLocal"
        Date -> Json.Encode.string "Date"
        Month -> Json.Encode.string "Month"
        Time -> Json.Encode.string "Time"
        Week -> Json.Encode.string "Week"
        Number -> Json.Encode.string "Number"
        Email -> Json.Encode.string "Email"
        Url -> Json.Encode.string "Url"
        Search -> Json.Encode.string "Search"
        Tel -> Json.Encode.string "Tel"
        Color -> Json.Encode.string "Color"



type alias SelectItem  =
   { selectItemID: String
   , selectItemText: String
   }

jsonDecSelectItem : Json.Decode.Decoder ( SelectItem )
jsonDecSelectItem =
   Json.Decode.succeed (\pselectItemID pselectItemText -> {selectItemID = pselectItemID, selectItemText = pselectItemText})
   |> required "selectItemID" (Json.Decode.string)
   |> required "selectItemText" (Json.Decode.string)

jsonEncSelectItem : SelectItem -> Value
jsonEncSelectItem  val =
   Json.Encode.object
   [ ("selectItemID", Json.Encode.string val.selectItemID)
   , ("selectItemText", Json.Encode.string val.selectItemText)
   ]



type FormGroupItem  =
    Input InputType
    | Select (List SelectItem)
    | TextArea Int

jsonDecFormGroupItem : Json.Decode.Decoder ( FormGroupItem )
jsonDecFormGroupItem =
    let jsonDecDictFormGroupItem = Dict.fromList
            [ ("Input", Json.Decode.lazy (\_ -> Json.Decode.map Input (jsonDecInputType)))
            , ("Select", Json.Decode.lazy (\_ -> Json.Decode.map Select (Json.Decode.list (jsonDecSelectItem))))
            , ("TextArea", Json.Decode.lazy (\_ -> Json.Decode.map TextArea (Json.Decode.int)))
            ]
    in  decodeSumObjectWithSingleField  "FormGroupItem" jsonDecDictFormGroupItem

jsonEncFormGroupItem : FormGroupItem -> Value
jsonEncFormGroupItem  val =
    let keyval v = case v of
                    Input v1 -> ("Input", encodeValue (jsonEncInputType v1))
                    Select v1 -> ("Select", encodeValue ((Json.Encode.list jsonEncSelectItem) v1))
                    TextArea v1 -> ("TextArea", encodeValue (Json.Encode.int v1))
    in encodeSumObjectWithSingleField keyval val



type alias FormGroupParams  =
   { label: (Maybe String)
   , item: FormGroupItem
   , help: (Maybe String)
   }

jsonDecFormGroupParams : Json.Decode.Decoder ( FormGroupParams )
jsonDecFormGroupParams =
   Json.Decode.succeed (\plabel pitem phelp -> {label = plabel, item = pitem, help = phelp})
   |> fnullable "label" (Json.Decode.string)
   |> required "item" (jsonDecFormGroupItem)
   |> fnullable "help" (Json.Decode.string)

jsonEncFormGroupParams : FormGroupParams -> Value
jsonEncFormGroupParams  val =
   Json.Encode.object
   [ ("label", (maybeEncode (Json.Encode.string)) val.label)
   , ("item", jsonEncFormGroupItem val.item)
   , ("help", (maybeEncode (Json.Encode.string)) val.help)
   ]



type CheckBoxParams  =
    CheckBoxParams String

jsonDecCheckBoxParams : Json.Decode.Decoder ( CheckBoxParams )
jsonDecCheckBoxParams =
    Json.Decode.lazy (\_ -> Json.Decode.map CheckBoxParams (Json.Decode.string))


jsonEncCheckBoxParams : CheckBoxParams -> Value
jsonEncCheckBoxParams (CheckBoxParams v1) =
    Json.Encode.string v1



type alias RadioItem  =
   { radioItemText: String
   , radioItemID: String
   }

jsonDecRadioItem : Json.Decode.Decoder ( RadioItem )
jsonDecRadioItem =
   Json.Decode.succeed (\pradioItemText pradioItemID -> {radioItemText = pradioItemText, radioItemID = pradioItemID})
   |> required "radioItemText" (Json.Decode.string)
   |> required "radioItemID" (Json.Decode.string)

jsonEncRadioItem : RadioItem -> Value
jsonEncRadioItem  val =
   Json.Encode.object
   [ ("radioItemText", Json.Encode.string val.radioItemText)
   , ("radioItemID", Json.Encode.string val.radioItemID)
   ]



type alias RadioListParams  =
   { legend: String
   , radioItems: (List RadioItem)
   }

jsonDecRadioListParams : Json.Decode.Decoder ( RadioListParams )
jsonDecRadioListParams =
   Json.Decode.succeed (\plegend pradioItems -> {legend = plegend, radioItems = pradioItems})
   |> required "legend" (Json.Decode.string)
   |> required "radioItems" (Json.Decode.list (jsonDecRadioItem))

jsonEncRadioListParams : RadioListParams -> Value
jsonEncRadioListParams  val =
   Json.Encode.object
   [ ("legend", Json.Encode.string val.legend)
   , ("radioItems", (Json.Encode.list jsonEncRadioItem) val.radioItems)
   ]



type alias FormEntry  =
   { entryID: String
   , formItem: FormItem
   }

jsonDecFormEntry : Json.Decode.Decoder ( FormEntry )
jsonDecFormEntry =
   Json.Decode.succeed (\pentryID pformItem -> {entryID = pentryID, formItem = pformItem})
   |> required "entryID" (Json.Decode.string)
   |> required "formItem" (jsonDecFormItem)

jsonEncFormEntry : FormEntry -> Value
jsonEncFormEntry  val =
   Json.Encode.object
   [ ("entryID", Json.Encode.string val.entryID)
   , ("formItem", jsonEncFormItem val.formItem)
   ]



type FormItem  =
    FormGroup FormGroupParams
    | CheckBox CheckBoxParams
    | RadioList RadioListParams

jsonDecFormItem : Json.Decode.Decoder ( FormItem )
jsonDecFormItem =
    let jsonDecDictFormItem = Dict.fromList
            [ ("FormGroup", Json.Decode.lazy (\_ -> Json.Decode.map FormGroup (jsonDecFormGroupParams)))
            , ("CheckBox", Json.Decode.lazy (\_ -> Json.Decode.map CheckBox (jsonDecCheckBoxParams)))
            , ("RadioList", Json.Decode.lazy (\_ -> Json.Decode.map RadioList (jsonDecRadioListParams)))
            ]
    in  decodeSumObjectWithSingleField  "FormItem" jsonDecDictFormItem

jsonEncFormItem : FormItem -> Value
jsonEncFormItem  val =
    let keyval v = case v of
                    FormGroup v1 -> ("FormGroup", encodeValue (jsonEncFormGroupParams v1))
                    CheckBox v1 -> ("CheckBox", encodeValue (jsonEncCheckBoxParams v1))
                    RadioList v1 -> ("RadioList", encodeValue (jsonEncRadioListParams v1))
    in encodeSumObjectWithSingleField keyval val



type alias FormParams  =
   { formEntries: (List FormEntry)
   , formEntriesResps: (List FormEntryRes)
   , formButtons: (List Button)
   , formTitle: (Maybe String)
   }

jsonDecFormParams : Json.Decode.Decoder ( FormParams )
jsonDecFormParams =
   Json.Decode.succeed (\pformEntries pformEntriesResps pformButtons pformTitle -> {formEntries = pformEntries, formEntriesResps = pformEntriesResps, formButtons = pformButtons, formTitle = pformTitle})
   |> required "formEntries" (Json.Decode.list (jsonDecFormEntry))
   |> required "formEntriesResps" (Json.Decode.list (jsonDecFormEntryRes))
   |> required "formButtons" (Json.Decode.list (jsonDecButton))
   |> fnullable "formTitle" (Json.Decode.string)

jsonEncFormParams : FormParams -> Value
jsonEncFormParams  val =
   Json.Encode.object
   [ ("formEntries", (Json.Encode.list jsonEncFormEntry) val.formEntries)
   , ("formEntriesResps", (Json.Encode.list jsonEncFormEntryRes) val.formEntriesResps)
   , ("formButtons", (Json.Encode.list jsonEncButton) val.formButtons)
   , ("formTitle", (maybeEncode (Json.Encode.string)) val.formTitle)
   ]



type FormItemRes  =
    IntRes Int
    | BoolRes Bool
    | TextRes String

jsonDecFormItemRes : Json.Decode.Decoder ( FormItemRes )
jsonDecFormItemRes =
    let jsonDecDictFormItemRes = Dict.fromList
            [ ("IntRes", Json.Decode.lazy (\_ -> Json.Decode.map IntRes (Json.Decode.int)))
            , ("BoolRes", Json.Decode.lazy (\_ -> Json.Decode.map BoolRes (Json.Decode.bool)))
            , ("TextRes", Json.Decode.lazy (\_ -> Json.Decode.map TextRes (Json.Decode.string)))
            ]
    in  decodeSumObjectWithSingleField  "FormItemRes" jsonDecDictFormItemRes

jsonEncFormItemRes : FormItemRes -> Value
jsonEncFormItemRes  val =
    let keyval v = case v of
                    IntRes v1 -> ("IntRes", encodeValue (Json.Encode.int v1))
                    BoolRes v1 -> ("BoolRes", encodeValue (Json.Encode.bool v1))
                    TextRes v1 -> ("TextRes", encodeValue (Json.Encode.string v1))
    in encodeSumObjectWithSingleField keyval val



type alias FormEntryRes  =
   { entryID: String
   , result: FormItemRes
   }

jsonDecFormEntryRes : Json.Decode.Decoder ( FormEntryRes )
jsonDecFormEntryRes =
   Json.Decode.succeed (\pentryID presult -> {entryID = pentryID, result = presult})
   |> required "entryID" (Json.Decode.string)
   |> required "result" (jsonDecFormItemRes)

jsonEncFormEntryRes : FormEntryRes -> Value
jsonEncFormEntryRes  val =
   Json.Encode.object
   [ ("entryID", Json.Encode.string val.entryID)
   , ("result", jsonEncFormItemRes val.result)
   ]



type alias FormRespParams  =
   { buttonClicked: String
   , entriesRes: (List FormEntryRes)
   }

jsonDecFormRespParams : Json.Decode.Decoder ( FormRespParams )
jsonDecFormRespParams =
   Json.Decode.succeed (\pbuttonClicked pentriesRes -> {buttonClicked = pbuttonClicked, entriesRes = pentriesRes})
   |> required "buttonClicked" (Json.Decode.string)
   |> required "entriesRes" (Json.Decode.list (jsonDecFormEntryRes))

jsonEncFormRespParams : FormRespParams -> Value
jsonEncFormRespParams  val =
   Json.Encode.object
   [ ("buttonClicked", Json.Encode.string val.buttonClicked)
   , ("entriesRes", (Json.Encode.list jsonEncFormEntryRes) val.entriesRes)
   ]



type alias Button  =
   { btnId: String
   , btnCaption: String
   , btnStyle: Style
   }

jsonDecButton : Json.Decode.Decoder ( Button )
jsonDecButton =
   Json.Decode.succeed (\pbtnId pbtnCaption pbtnStyle -> {btnId = pbtnId, btnCaption = pbtnCaption, btnStyle = pbtnStyle})
   |> required "btnId" (Json.Decode.string)
   |> required "btnCaption" (Json.Decode.string)
   |> required "btnStyle" (jsonDecStyle)

jsonEncButton : Button -> Value
jsonEncButton  val =
   Json.Encode.object
   [ ("btnId", Json.Encode.string val.btnId)
   , ("btnCaption", Json.Encode.string val.btnCaption)
   , ("btnStyle", jsonEncStyle val.btnStyle)
   ]



type alias ButtonBarParams  =
   { bbPrompt: String
   , bbButtons: (List Button)
   }

jsonDecButtonBarParams : Json.Decode.Decoder ( ButtonBarParams )
jsonDecButtonBarParams =
   Json.Decode.succeed (\pbbPrompt pbbButtons -> {bbPrompt = pbbPrompt, bbButtons = pbbButtons})
   |> required "bbPrompt" (Json.Decode.string)
   |> required "bbButtons" (Json.Decode.list (jsonDecButton))

jsonEncButtonBarParams : ButtonBarParams -> Value
jsonEncButtonBarParams  val =
   Json.Encode.object
   [ ("bbPrompt", Json.Encode.string val.bbPrompt)
   , ("bbButtons", (Json.Encode.list jsonEncButton) val.bbButtons)
   ]



type alias StaticBarParams  =
   { staticBarNotice: String
   }

jsonDecStaticBarParams : Json.Decode.Decoder ( StaticBarParams )
jsonDecStaticBarParams =
   Json.Decode.succeed (\pstaticBarNotice -> {staticBarNotice = pstaticBarNotice}) |> custom (Json.Decode.string)

jsonEncStaticBarParams : StaticBarParams -> Value
jsonEncStaticBarParams  val =
   Json.Encode.string val.staticBarNotice


type UICard  =
    ButtonBar ButtonBarParams
    | StaticBar StaticBarParams
    | Form FormParams

jsonDecUICard : Json.Decode.Decoder ( UICard )
jsonDecUICard =
    let jsonDecDictUICard = Dict.fromList
            [ ("ButtonBar", Json.Decode.lazy (\_ -> Json.Decode.map ButtonBar (jsonDecButtonBarParams)))
            , ("StaticBar", Json.Decode.lazy (\_ -> Json.Decode.map StaticBar (jsonDecStaticBarParams)))
            , ("Form", Json.Decode.lazy (\_ -> Json.Decode.map Form (jsonDecFormParams)))
            ]
    in  decodeSumObjectWithSingleField  "UICard" jsonDecDictUICard

jsonEncUICard : UICard -> Value
jsonEncUICard  val =
    let keyval v = case v of
                    ButtonBar v1 -> ("ButtonBar", encodeValue (jsonEncButtonBarParams v1))
                    StaticBar v1 -> ("StaticBar", encodeValue (jsonEncStaticBarParams v1))
                    Form v1 -> ("Form", encodeValue (jsonEncFormParams v1))
    in encodeSumObjectWithSingleField keyval val



type UIResp  =
    ButtonClickedResp String
    | FormResp FormRespParams

jsonDecUIResp : Json.Decode.Decoder ( UIResp )
jsonDecUIResp =
    let jsonDecDictUIResp = Dict.fromList
            [ ("ButtonClickedResp", Json.Decode.lazy (\_ -> Json.Decode.map ButtonClickedResp (Json.Decode.string)))
            , ("FormResp", Json.Decode.lazy (\_ -> Json.Decode.map FormResp (jsonDecFormRespParams)))
            ]
    in  decodeSumObjectWithSingleField  "UIResp" jsonDecDictUIResp

jsonEncUIResp : UIResp -> Value
jsonEncUIResp  val =
    let keyval v = case v of
                    ButtonClickedResp v1 -> ("ButtonClickedResp", encodeValue (Json.Encode.string v1))
                    FormResp v1 -> ("FormResp", encodeValue (jsonEncFormRespParams v1))
    in encodeSumObjectWithSingleField keyval val



type ServerReq  =
    CreateEntry EntryID EntryTitle ReqID UICard
    | DeleteEntry EntryID
    | ReplaceEntry EntryID ReqID UICard

jsonDecServerReq : Json.Decode.Decoder ( ServerReq )
jsonDecServerReq =
    let jsonDecDictServerReq = Dict.fromList
            [ ("CreateEntry", Json.Decode.lazy (\_ -> Json.Decode.map4 CreateEntry (Json.Decode.index 0 (jsonDecEntryID)) (Json.Decode.index 1 (jsonDecEntryTitle)) (Json.Decode.index 2 (jsonDecReqID)) (Json.Decode.index 3 (jsonDecUICard))))
            , ("DeleteEntry", Json.Decode.lazy (\_ -> Json.Decode.map DeleteEntry (jsonDecEntryID)))
            , ("ReplaceEntry", Json.Decode.lazy (\_ -> Json.Decode.map3 ReplaceEntry (Json.Decode.index 0 (jsonDecEntryID)) (Json.Decode.index 1 (jsonDecReqID)) (Json.Decode.index 2 (jsonDecUICard))))
            ]
    in  decodeSumObjectWithSingleField  "ServerReq" jsonDecDictServerReq

jsonEncServerReq : ServerReq -> Value
jsonEncServerReq  val =
    let keyval v = case v of
                    CreateEntry v1 v2 v3 v4 -> ("CreateEntry", encodeValue (Json.Encode.list identity [jsonEncEntryID v1, jsonEncEntryTitle v2, jsonEncReqID v3, jsonEncUICard v4]))
                    DeleteEntry v1 -> ("DeleteEntry", encodeValue (jsonEncEntryID v1))
                    ReplaceEntry v1 v2 v3 -> ("ReplaceEntry", encodeValue (Json.Encode.list identity [jsonEncEntryID v1, jsonEncReqID v2, jsonEncUICard v3]))
    in encodeSumObjectWithSingleField keyval val



type ClientResp  =
    ClientResp EntryID ReqID UIResp

jsonDecClientResp : Json.Decode.Decoder ( ClientResp )
jsonDecClientResp =
    Json.Decode.lazy (\_ -> Json.Decode.map3 ClientResp (Json.Decode.index 0 (jsonDecEntryID)) (Json.Decode.index 1 (jsonDecReqID)) (Json.Decode.index 2 (jsonDecUIResp)))


jsonEncClientResp : ClientResp -> Value
jsonEncClientResp (ClientResp v1 v2 v3) =
    Json.Encode.list identity [jsonEncEntryID v1, jsonEncReqID v2, jsonEncUIResp v3]

