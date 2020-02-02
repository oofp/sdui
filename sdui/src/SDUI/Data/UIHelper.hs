module SDUI.Data.UIHelper where 

import Protolude
import SDUI.Data.SDUIData

mkBtnBar :: Text -> [Text] -> UICard
mkBtnBar prompt btns =
  ButtonBar $ ButtonBarParams prompt $ fmap (\txt -> Button txt txt) btns

mkStaticBar :: Text -> UICard
mkStaticBar txt = StaticBar $ StaticBarParams txt
