module SDUIView exposing(..)

import SDUI exposing (..)
import SDUIModel exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h1, input, p, span, text, br)
import Html.Events exposing (onClick, onInput)

viewSDUI : SDUIModel -> (ClientResp -> msg) -> Html msg
viewSDUI sduiModel respMsgFunc = 
    div [] (List.map (viewEntry respMsgFunc) (Dict.values sduiModel))


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

viewBBButton : (ClientResp -> msg) -> SDUIEntry -> Button -> Html msg
viewBBButton respMsgFunc sduiEntry bbButton = 
    button [onClick (respMsgFunc (ClientResp sduiEntry.entryID sduiEntry.reqID (ButtonClickedResp bbButton.btnId) )) ] [text bbButton.btnCaption]
