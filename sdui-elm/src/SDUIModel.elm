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
        

deleteEntry : EntryID ->SDUIModel -> SDUIModel
deleteEntry entryID = Dict.remove entryID  