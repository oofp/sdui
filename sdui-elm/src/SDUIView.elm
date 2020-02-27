module SDUIView exposing(..)

import SDUI exposing (..)
import SDUIModel exposing (..)
import SDUIViewUtils exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h1, input, p, span, text, br)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (attribute, class, selected, checked, disabled, href, size, style, type_, value, for)
import Bootstrap.Grid as Grid
import Bootstrap.Card as Card
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Card.Block as Block
import Bootstrap.Spinner as Spinner
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Form.Checkbox as Checkbox
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Radio as Radio
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Button as Button

viewSDUI : SDUIModel -> (ClientResp -> msg) -> (UpdateFormParams -> msg) -> List (Html msg)
viewSDUI sduiModel respMsgFunc updateFormMsg = 
    List.map (viewEntry respMsgFunc updateFormMsg) (Dict.values sduiModel)

convertFormMsgFunc : (UpdateFormParams -> msg) -> SDUIEntry -> String -> FormItemRes ->  msg
convertFormMsgFunc updateFormMsg sduiEntry formEntryID formItemRes =
    updateFormMsg 
        { entryID = sduiEntry.entryID 
        , reqID = sduiEntry.reqID
        , formEntryID = formEntryID
        , newResp = formItemRes
        }

viewEntry : (ClientResp -> msg) -> (UpdateFormParams -> msg) -> SDUIEntry -> Html msg
viewEntry respMsgFunc updateFormMsg sduiEntry  = 
        Card.config [Card.light] 
            |> Card.headerH6 [class "bg-primary text-white"] [text sduiEntry.entryTitle]
            |> Card.block [] 
                [Block.custom  (viewEntryContent respMsgFunc (convertFormMsgFunc updateFormMsg sduiEntry) sduiEntry)]
            |> Card.view 
                    

viewEntryContent : (ClientResp -> msg) -> (String -> FormItemRes ->  msg) -> SDUIEntry -> Html msg
viewEntryContent respMsgFunc updateFormMsgFunc sduiEntry = 
    case sduiEntry.uiCard of
        Nothing -> 
            div []
                [ div [] [text "Please wait"] 
                , Spinner.spinner [ Spinner.grow, Spinner.large ] []
                ]

        Just (ButtonBar bbParams) -> 
            div []
                [ text bbParams.bbPrompt
                , div [class "btn-toolbar  pull-right", attribute "role" "toolbar" ]
                    (List.map (viewBBButton respMsgFunc sduiEntry) bbParams.bbButtons)
                ] 
        Just (StaticBar sbParams) -> 
            text sbParams.staticBarNotice

        Just (Form formParams) -> -- add buttons
            Form.form [] 
                (List.map (viewFormEntry respMsgFunc updateFormMsgFunc formParams) formParams.formEntries ++
                viewFormButtons respMsgFunc sduiEntry formParams.formButtons formParams)


viewBBButton : (ClientResp -> msg) -> SDUIEntry -> Button -> Html msg
viewBBButton respMsgFunc sduiEntry bbButton = 
    Button.button [buttonRole bbButton.btnStyle, Button.attrs [Spacing.mr2], Button.onClick (respMsgFunc (ClientResp sduiEntry.entryID sduiEntry.reqID (ButtonClickedResp bbButton.btnId) )) ] [text bbButton.btnCaption]

viewFormButtons : (ClientResp -> msg) -> SDUIEntry -> List Button -> FormParams -> List (Html msg)
viewFormButtons respMsgFunc sduiEntry formButtons formParams =
    List.map (viewFormButton respMsgFunc sduiEntry formParams) formButtons

viewFormButton : (ClientResp -> msg) -> SDUIEntry -> FormParams -> Button -> Html msg
viewFormButton respMsgFunc sduiEntry formParams bbButton = 
    Button.button 
            [ buttonRole bbButton.btnStyle  
            , Button.onClick (respMsgFunc (ClientResp sduiEntry.entryID sduiEntry.reqID (FormResp (FormRespParams bbButton.btnId formParams.formEntriesResps)))) 
            , Button.attrs [Spacing.mr2]
            ] [text bbButton.btnCaption]

viewFormEntry : (ClientResp -> msg) -> (String -> FormItemRes ->  msg) -> FormParams -> FormEntry -> Html msg
viewFormEntry respMsgFunc updateFormMsgFunc formParams formEntry = 
    case formEntry.formItem of 
        FormGroup params -> viewFormGroup updateFormMsgFunc formParams formEntry.entryID params  
        CheckBox params -> viewCheckBox  updateFormMsgFunc formParams formEntry.entryID params
        RadioList  params -> viewRadioList  updateFormMsgFunc formParams formEntry.entryID params

viewFormGroup : (String -> FormItemRes ->  msg) -> FormParams -> String -> FormGroupParams -> Html msg
viewFormGroup updateFormMsgFunc formParams entryID params = 
    Form.group [] 
        ( List.concat   [ viewFormLabel entryID params.label
                        , [viewFormItem updateFormMsgFunc formParams entryID params.item]
                        , viewFormHelp params.help]) 

viewFormLabel : String -> Maybe String -> List (Html msg)
viewFormLabel entryID lblMaybe = case lblMaybe of 
    Just l -> [Form.label [for entryID] [ text l]]
    Nothing -> []

viewFormHelp : Maybe String -> List (Html msg)
viewFormHelp helpMaybe = case helpMaybe of 
    Just h -> [Form.help [] [ text h]]
    Nothing -> []

viewFormItem : (String -> FormItemRes ->  msg) -> FormParams -> String -> FormGroupItem -> Html msg
viewFormItem updateFormMsgFunc formParams entryID groupItem = 
    case groupItem of 
        Input inputType -> viewInputItem updateFormMsgFunc formParams entryID inputType 
        Select selectItems -> viewSelectItems updateFormMsgFunc formParams entryID selectItems  
        TextArea rows -> viewTextArea updateFormMsgFunc formParams entryID rows

viewInputItem : (String -> FormItemRes ->  msg) -> FormParams -> String -> InputType -> Html msg
viewInputItem updateFormMsgFunc formParams entryID inputType =  
    let 
        -- inpType : InputType -> List (Option msg) -> Html msg
        inpType inType = case inType of 
            Text -> Input.text
            Password -> Input.password
            DateTimeLocal -> Input.datetimeLocal
            Date -> Input.date
            Month -> Input.month
            Time -> Input.time
            Week -> Input.week
            Number -> Input.number
            Email -> Input.email
            Url -> Input.url
            Search -> Input.search
            Tel -> Input.tel
            Color -> Input.color
    in       
        inpType inputType
            [ Input.id entryID
            , Input.value (findFormEntryResText entryID formParams)
            , Input.onInput (\txt -> 
                if inputType == Number
                    then updateFormMsgFunc entryID (IntRes (String.toInt txt |> Maybe.withDefault 0)) 
                    else updateFormMsgFunc entryID (TextRes txt))  
            ]

viewSelectItems : (String -> FormItemRes ->  msg) -> FormParams -> String -> List SelectItem -> Html msg
viewSelectItems updateFormMsgFunc formParams entryID selectItems =  
    Select.select 
        [ Select.id entryID
        , Select.onChange (\txt -> updateFormMsgFunc entryID (TextRes txt))  
        ]
        (List.map (\i-> 
            Select.item [ value i.selectItemID
                        , selected (i.selectItemID == findFormEntryResText entryID formParams)
                        ] 
                        [text i.selectItemText]) selectItems)

viewTextArea : (String -> FormItemRes ->  msg) -> FormParams -> String -> Int -> Html msg
viewTextArea updateFormMsgFunc formParams entryID rows =  
    Textarea.textarea 
        [ Textarea.id entryID
        , Textarea.rows rows
        , Textarea.value (findFormEntryResText entryID formParams)
        , Textarea.onInput (\txt -> updateFormMsgFunc entryID (TextRes txt))  
        ]

viewCheckBox : (String -> FormItemRes ->  msg) -> FormParams -> String -> CheckBoxParams -> Html msg
viewCheckBox updateFormMsgFunc formParams entryID (CheckBoxParams txt) = 
    Checkbox.checkbox 
        [ Checkbox.id entryID 
        , Checkbox.checked (findFormEntryResBool entryID formParams) 
        , Checkbox.onCheck (\fl-> updateFormMsgFunc entryID (BoolRes fl))] txt

viewRadioList : (String -> FormItemRes ->  msg) -> FormParams -> String -> RadioListParams -> Html msg
viewRadioList updateFormMsgFunc formParams entryID params = 
    Fieldset.config
            |> Fieldset.asGroup
            |> Fieldset.legend [] [ text params.legend ]
            |> Fieldset.children
                ( Radio.radioList entryID
                    (List.map 
                        (\item -> Radio.create 
                                    [ Radio.id item.radioItemID
                                    , Radio.onClick (updateFormMsgFunc entryID (TextRes item.radioItemID))  
                                    , Radio.checked (item.radioItemID == findFormEntryResText entryID formParams)
                                    ] item.radioItemText) params.radioItems)
                )
            |> Fieldset.view
