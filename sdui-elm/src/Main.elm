port module Main exposing (main)

{-| WebSocketClient Example
-}

import Browser
import Cmd.Extra exposing (addCmd, addCmds, withCmd, withCmds, withNoCmd)
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, h1, h4, input, p, span, text)
import Html.Attributes exposing (class, checked, disabled, href, size, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode exposing (Value, encode)
import Json.Decode exposing (decodeString, errorToString)
import PortFunnel.WebSocket as WebSocket exposing (Response(..))
import PortFunnels exposing (FunnelDict, Handler(..), State)
import SDUI exposing (..)
import SDUIModel exposing (..)
import SDUIView exposing (..)
-- import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Task as Task
import Time as Time

{- This section contains boilerplate that you'll always need.

   First, copy PortFunnels.elm into your project, and modify it
   to support all the funnel modules you use.

   Then update the `handlers` list with an entry for each funnel.

   Those handler functions are the meat of your interaction with each
   funnel module.
-}

handlers : List (Handler Model Msg)
handlers =
    [ WebSocketHandler socketHandler
    ]


subscriptions : Model -> Sub Msg
subscriptions =
    PortFunnels.subscriptions Process


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict handlers getCmdPort


{-| Get a possibly simulated output port.
-}
getCmdPort : String -> Model -> (Value -> Cmd Msg)
getCmdPort moduleName model =
    PortFunnels.getCmdPort Process moduleName False


{-| The real output port.
-}
cmdPort : Value -> Cmd Msg
cmdPort =
    PortFunnels.getCmdPort Process "" False



-- MODEL


defaultUrl : String
defaultUrl =
    "ws://localhost:8072"


type alias Model =
    { sduiModel : SDUIModel
    , url : String
    , log : List String
    , wasLoaded : Bool
    , state : State
    , key : String
    , error : Maybe String
    , initConnect : Bool
    }


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let model =
            { sduiModel = SDUIModel.emptyModel
            , url = defaultUrl
            , log = []
            , wasLoaded = False
            , state = PortFunnels.initialState
            , key = "socket"
            , error = Nothing
            , initConnect = False
            }
    in model |> withNoCmd    
       -- (model , fireInitMsg) 
        --|> withCmd -- connect on startup
        --    (WebSocket.makeOpenWithKey model.key model.url
        --        |> send model
        --    )

fireInitMsg : Cmd Msg
fireInitMsg = Task.perform (\_ -> Connect) Time.now

connect : Model -> (Model , Cmd Msg)
connect model = 
            { model
                | log = ("Connecting to " ++ model.url) :: model.log
            }
                |> withCmd
                    (WebSocket.makeOpenWithKey model.key model.url
                        |> send model
                    )

-- UPDATE

type Msg
    = Send ClientResp
    | Process Value
    | Connect

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Connect ->
            { model
                | log = ("Connecting to " ++ model.url) :: model.log
            }
                |> withCmd
                    (WebSocket.makeOpenWithKey model.key model.url
                        |> send model
                    )
        Send (ClientResp entryID reqID uiResp) ->
            case clearEntry entryID reqID model.sduiModel of 
                Nothing -> model |> withNoCmd
                Just newSDUIModel ->
                    let msgStr = encode 1 (jsonEncClientResp (ClientResp entryID reqID uiResp))
                    in 
                        { model |
                                sduiModel = newSDUIModel,
                                log = ("Sending \"" ++ msgStr ++ "\"") :: model.log
                        }
                            |> withCmd
                                (WebSocket.makeSend model.key msgStr
                                    |> send model
                                )

        Process value ->
            case
                PortFunnels.processValue funnelDict value model.state model
            of
                Err error ->
                    { model | error = Just error } |> withNoCmd

                Ok res ->
                    let (mdl, cmd) = res
                    in 
                        if mdl.initConnect
                            then (mdl,cmd)
                            else 
                                let newModel = {mdl | initConnect = True}
                                in (newModel, 
                                    Cmd.batch [WebSocket.makeOpenWithKey model.key model.url
                                                    |> send newModel , cmd])
                                             


send : Model -> WebSocket.Message -> Cmd Msg
send model message =
    WebSocket.send (getCmdPort WebSocket.moduleName model) message

handleIncomingMsg : String -> Model -> Model 
handleIncomingMsg msg model =
    case decodeString jsonDecServerReq msg of
        Err err -> {model | log = ("Error decoding incominf msg: \"" ++ errorToString err ) :: model.log} 
        Ok serverReq -> {model | sduiModel = updateSDUIWithServerReq serverReq model.sduiModel}  

updateSDUIWithServerReq : ServerReq -> SDUIModel -> SDUIModel 
updateSDUIWithServerReq serverReq sduiModel = 
    case serverReq of
        CreateEntry entryID entryTitle reqID uiCard -> 
            let newEntry = { entryID = entryID
                            , entryTitle = entryTitle
                            , reqID = reqID
                            , uiCard = Just uiCard 
                            }
            in addEntry newEntry sduiModel                
        DeleteEntry entryID -> deleteEntry entryID sduiModel
        ReplaceEntry entryID reqID uiCard -> updateEntry entryID reqID uiCard sduiModel

doIsLoaded : Model -> Model
doIsLoaded model =
    if not model.wasLoaded && WebSocket.isLoaded model.state.websocket then
        { model
            | wasLoaded = True
        }

    else
        model


socketHandler : Response -> State -> Model -> ( Model, Cmd Msg )
socketHandler response state mdl =
    let
        model =
            doIsLoaded
                { mdl
                    | state = state
                    , error = Nothing
                }
    in
    case response of
        WebSocket.MessageReceivedResponse { message } ->
            { model | log = ("Received \"" ++ message ++ "\"") :: model.log}
                |> handleIncomingMsg message |> withNoCmd

        WebSocket.ConnectedResponse r ->
            { model | log = ("Connected: " ++ r.description) :: model.log }
                |> withNoCmd

        WebSocket.ClosedResponse { code, wasClean, expected } ->
            { model
                | log =
                    ("Closed, " ++ closedString code wasClean expected)
                        :: model.log
            }
                |> withNoCmd

        WebSocket.ErrorResponse error ->
            { model | log = WebSocket.errorToString error :: model.log }
                |> withNoCmd

        _ ->
            case WebSocket.reconnectedResponses response of
                [] ->
                    model |> withNoCmd

                [ ReconnectedResponse r ] ->
                    { model | log = ("Reconnected: " ++ r.description) :: model.log }
                        |> withNoCmd

                list ->
                    { model | log = Debug.toString list :: model.log }
                        |> withNoCmd


closedString : WebSocket.ClosedCode -> Bool -> Bool -> String
closedString code wasClean expected =
    "code: "
        ++ WebSocket.closedCodeToString code
        ++ ", "
        ++ (if wasClean then
                "clean"

            else
                "not clean"
           )
        ++ ", "
        ++ (if expected then
                "expected"

            else
                "NOT expected"
           )



-- VIEW


b : String -> Html Msg
b string =
    Html.b [] [ text string ]


br : Html msg
br =
    Html.br [] []


docp : String -> Html Msg
docp string =
    p [] [ text string ]


view : Model -> Html Msg
view model =
    let
        isConnected =
            WebSocket.isConnected model.key model.state.websocket
    in
        Grid.container []
            (( Card.config [] 
                |> Card.headerH6 [class "bg-success text-white"] [text "SDUIExample"]
                |> Card.block []
                    [ Block.custom 
                        ( p []
                            [ b "url: "
                            , input
                                [ value model.url
                                , size 30
                                , disabled True
                                ]
                                []
                            , button [ onClick Connect,  disabled isConnected ]
                                    [ text "Connect" ]
                            ]        
                        )
                    ]
                |> Card.view
             ) :: (viewSDUI model.sduiModel Send))        
    -- div [class "container"]
    {-
        [ div [ class "panel panel-primary" ]
            [ div [ class "panel-heading" ]
                [ text "SDUI Example"]
            , div [ class "panel-body" ]    
             ]
        , 
        ] 
        -}

        {-
        , p [] <|
            List.concat
                [ [ b "Log:"
                  , br
                  ]
                , List.intersperse br (List.map text model.log)
                ]
        -}        

