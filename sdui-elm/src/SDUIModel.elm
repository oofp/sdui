module SDUIModel exposing(..)

import SDUI exposing (..)
import Dict exposing (Dict)

type alias SDUIEntry =
    { entryID : EntryID
    , entryTitle : EntryTitle
    , reqID : ReqID
    , uiCard : Maybe UICard 
    }

type alias SDUIModel = Dict String SDUIEntry

type alias UpdateFormParams = {entryID : EntryID, reqID : ReqID, formEntryID : String , newResp : FormItemRes} 

emptyModel : SDUIModel
emptyModel = Dict.empty

addEntry : SDUIEntry -> SDUIModel -> SDUIModel
addEntry entry entries = Dict.insert entry.entryID entry entries 

updateEntry : EntryID -> ReqID -> UICard -> SDUIModel -> SDUIModel
updateEntry entryID reqID uiCard entries = Dict.update entryID (updateEntryFunc reqID uiCard) entries 

updateEntryFunc : ReqID -> UICard -> Maybe SDUIEntry -> Maybe SDUIEntry
updateEntryFunc reqID uiCard maybeEntry = Maybe.map (\curEntry -> {curEntry | reqID = reqID, uiCard = Just uiCard}) maybeEntry 

-- return Nothing if model has not been updated  
clearEntry : EntryID -> ReqID -> SDUIModel -> Maybe SDUIModel
clearEntry entryID reqID entries = 
    let curEntry = Dict.get entryID entries
    in Maybe.andThen (\sduiEntry -> 
        sduiEntry.uiCard |> Maybe.andThen (\_ -> 
            if reqID == sduiEntry.reqID 
                then Just (Dict.insert entryID {sduiEntry | uiCard = Nothing} entries)
                else Nothing)) curEntry
        

deleteEntry : EntryID -> SDUIModel -> SDUIModel
deleteEntry entryID = Dict.remove entryID  

--
updateEntryCard : EntryID -> ReqID -> (UICard -> UICard) -> SDUIModel -> SDUIModel
updateEntryCard entryID reqID uiCardUpdateFunc entries = Dict.update entryID (updateEntryCardFunc reqID uiCardUpdateFunc) entries 

updateEntryCardFunc : ReqID -> (UICard -> UICard)  -> Maybe SDUIEntry -> Maybe SDUIEntry
updateEntryCardFunc reqID uiCardUpdateFunc maybeEntry = 
    Maybe.map (\curEntry -> 
        if (curEntry.reqID == reqID )
            then {curEntry | uiCard = Maybe.map uiCardUpdateFunc curEntry.uiCard}
            else curEntry) maybeEntry 


updateFormCard : String -> FormItemRes -> UICard -> UICard
updateFormCard formEntryID newResp uiCard =
    case uiCard of
        Form formParams -> Form {formParams | formEntriesResps = updateFormRespList formEntryID newResp formParams.formEntriesResps}
        _ -> uiCard

updateFormRespList : String -> FormItemRes -> List FormEntryRes -> List FormEntryRes
updateFormRespList formEntryID newResp curList = case curList of 
    [] -> []
    (head :: tail) -> 
        if head.entryID == formEntryID
            then FormEntryRes formEntryID newResp :: tail
            else head :: updateFormRespList formEntryID newResp tail

updateForm : UpdateFormParams -> SDUIModel -> SDUIModel                                      
updateForm params model = 
    updateEntryCard params.entryID params.reqID (updateFormCard params.formEntryID params.newResp) model


findFormEntryResp : String -> FormParams -> Maybe FormItemRes
findFormEntryResp formEntryID formParams = 
    List.filter (\r -> r.entryID == formEntryID) formParams.formEntriesResps |> List.head |> Maybe.map (\r -> r.result)

findFormEntryResText : String -> FormParams -> String
findFormEntryResText formEntryID formParams = case findFormEntryResp formEntryID formParams of 
    (Just (TextRes txt)) -> txt
    (Just (IntRes i)) -> String.fromInt i
    _ -> ""    

findFormEntryResBool : String -> FormParams -> Bool
findFormEntryResBool formEntryID formParams = case findFormEntryResp formEntryID formParams of 
    (Just (BoolRes fl)) -> fl
    _ -> False        

findFormEntryResNum : String -> FormParams -> Int
findFormEntryResNum formEntryID formParams = case findFormEntryResp formEntryID formParams of 
    (Just (IntRes i)) -> i
    _ -> 0            

