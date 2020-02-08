module SDUIView exposing(..)

import SDUI exposing (..)
import SDUIModel exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h1, input, p, span, text, br)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (attribute, class, checked, disabled, href, size, style, type_, value)

viewSDUI : SDUIModel -> (ClientResp -> msg) -> Html msg
viewSDUI sduiModel respMsgFunc = 
    div [] (List.map (viewEntry respMsgFunc) (Dict.values sduiModel))

{-
viewEntry : (ClientResp -> msg) -> SDUIEntry -> Html msg
viewEntry respMsgFunc sduiEntry = 
    case sduiEntry.uiCard of
        Nothing -> div [] [text "Please wait"]
        Just (ButtonBar bbParams) -> 
            div []
                [ text sduiEntry.entryTitle
                , br [] []
                , text bbParams.bbPrompt
                , div [] (List.map (viewBBButton respMsgFunc sduiEntry) bbParams.bbButtons) 
                ] 
        Just (StaticBar sbParams) -> 
            div []
                [ text sduiEntry.entryTitle
                , br [] []
                , text sbParams.staticBarNotice
                ] 

viewBBButton : (ClientResp -> msg) -> SDUIEntry -> Button -> Html msg
viewBBButton respMsgFunc sduiEntry bbButton = 
    button [onClick (respMsgFunc (ClientResp sduiEntry.entryID sduiEntry.reqID (ButtonClickedResp bbButton.btnId) )) ] [text bbButton.btnCaption]
-}

viewEntry : (ClientResp -> msg) -> SDUIEntry -> Html msg
viewEntry respMsgFunc sduiEntry = 
        div [ class "panel panel-primary" ]
            [ div [ class "panel-heading" ]
                [ text sduiEntry.entryTitle]
            , div [ class "panel-body" ] [viewEntryContent respMsgFunc sduiEntry]
            ]   

viewEntryContent : (ClientResp -> msg) -> SDUIEntry -> Html msg
viewEntryContent respMsgFunc sduiEntry = 
    case sduiEntry.uiCard of
        Nothing -> text "Please wait" 

        Just (ButtonBar bbParams) -> 
            div []
                [ text bbParams.bbPrompt
                , div [class "btn-toolbar  pull-right", attribute "role" "toolbar" ]
                    (List.map (viewBBButton respMsgFunc sduiEntry) bbParams.bbButtons)
                ] 
        Just (StaticBar sbParams) -> 
            text sbParams.staticBarNotice

viewBBButton : (ClientResp -> msg) -> SDUIEntry -> Button -> Html msg
viewBBButton respMsgFunc sduiEntry bbButton = 
    button [class "btn btn-default", onClick (respMsgFunc (ClientResp sduiEntry.entryID sduiEntry.reqID (ButtonClickedResp bbButton.btnId) )) ] [text bbButton.btnCaption]
