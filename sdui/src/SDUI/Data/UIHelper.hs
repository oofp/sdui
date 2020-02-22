module SDUI.Data.UIHelper where 

import            Protolude
import            SDUI.Data.SDUIData
import            SDUI.Data.Button
import qualified SDUI.Data.Style as Style
import            Data.List (findIndex)
import            Data.Maybe (fromMaybe)

mkBtnBar :: Text -> [Text] -> UICard
mkBtnBar prompt btns =
  ButtonBar $ ButtonBarParams prompt $ fmap (\txt -> Button txt txt Style.Primary) btns

mkStaticBar :: Text -> UICard
mkStaticBar txt = StaticBar $ StaticBarParams txt


uiRespIndex :: UICard -> UIResp -> Int
uiRespIndex (ButtonBar (ButtonBarParams _ buttons)) (ButtonClickedResp respText) = 
    fromMaybe 0 (findIndex (\btn -> btnId btn == respText) buttons) 
uiRespIndex _ _ = 0    