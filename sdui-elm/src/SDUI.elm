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



type alias Button  =
   { btnId: String
   , btnCaption: String
   }

jsonDecButton : Json.Decode.Decoder ( Button )
jsonDecButton =
   Json.Decode.succeed (\pbtnId pbtnCaption -> {btnId = pbtnId, btnCaption = pbtnCaption})
   |> required "btnId" (Json.Decode.string)
   |> required "btnCaption" (Json.Decode.string)

jsonEncButton : Button -> Value
jsonEncButton  val =
   Json.Encode.object
   [ ("btnId", Json.Encode.string val.btnId)
   , ("btnCaption", Json.Encode.string val.btnCaption)
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

jsonDecUICard : Json.Decode.Decoder ( UICard )
jsonDecUICard =
    let jsonDecDictUICard = Dict.fromList
            [ ("ButtonBar", Json.Decode.lazy (\_ -> Json.Decode.map ButtonBar (jsonDecButtonBarParams)))
            , ("StaticBar", Json.Decode.lazy (\_ -> Json.Decode.map StaticBar (jsonDecStaticBarParams)))
            ]
    in  decodeSumObjectWithSingleField  "UICard" jsonDecDictUICard

jsonEncUICard : UICard -> Value
jsonEncUICard  val =
    let keyval v = case v of
                    ButtonBar v1 -> ("ButtonBar", encodeValue (jsonEncButtonBarParams v1))
                    StaticBar v1 -> ("StaticBar", encodeValue (jsonEncStaticBarParams v1))
    in encodeSumObjectWithSingleField keyval val



type UIResp  =
    ButtonClickedResp String

jsonDecUIResp : Json.Decode.Decoder ( UIResp )
jsonDecUIResp =
    Json.Decode.lazy (\_ -> Json.Decode.map ButtonClickedResp (Json.Decode.string))


jsonEncUIResp : UIResp -> Value
jsonEncUIResp (ButtonClickedResp v1) =
    Json.Encode.string v1



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
