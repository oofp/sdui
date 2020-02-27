module SDUI.Data.UIHelper where 

import            Protolude
import            SDUI.Data.SDUIData
import            SDUI.Data.Button
import            SDUI.Data.Form
import            SDUI.Data.FormRes
import qualified SDUI.Data.Style as Style
import            Data.List (findIndex, find)
import            Data.Maybe (fromMaybe)

mkBtnBar :: Text -> [Text] -> UICard
mkBtnBar prompt btns =
  ButtonBar $ ButtonBarParams prompt $ fmap (\txt -> Button txt txt Style.Primary) btns

mkStaticBar :: Text -> UICard
mkStaticBar txt = StaticBar $ StaticBarParams txt


uiRespIndex :: UICard -> UIResp -> Int
uiRespIndex (ButtonBar (ButtonBarParams _ buttons)) (ButtonClickedResp respText) = 
    fromMaybe 0 (findIndex (\btn -> btnId btn == respText) buttons) 
uiRespIndex (Form (FormParams _ _ buttons)) (FormResp (FormRespParams buttonClicked _ )) = 
    fromMaybe 0 (findIndex (\btn -> btnId btn == buttonClicked) buttons) 

getFormRes :: UIResp -> Text -> Maybe FormItemRes
getFormRes (FormResp (FormRespParams _ entries)) eID = fmap result $ find (\e -> (SDUI.Data.FormRes.entryID e == eID)) entries
getFormRes _ _ =  Nothing

getFormTextRes :: UIResp -> Text -> Text
getFormTextRes uiResp eID = case getFormRes uiResp eID of 
  (Just (TextRes txt)) -> txt
  _ -> ""

getFormNumRes :: UIResp -> Text -> Int
getFormNumRes uiResp eID = case getFormRes uiResp eID of 
  (Just (IntRes i)) -> i
  _ -> 0

getFormBoolRes :: UIResp -> Text -> Bool
getFormBoolRes uiResp eID = case getFormRes uiResp eID of 
  (Just (BoolRes fl)) -> fl
  _ -> False
