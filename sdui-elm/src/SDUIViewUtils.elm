module SDUIViewUtils exposing(..)

import SDUI exposing (..)
import Bootstrap.Button as Button

--buttonRole : Style -> Option msg
buttonRole style = case style of 
    Primary -> Button.primary
    Secondary -> Button.secondary
    Success -> Button.success
    Info -> Button.info
    Warning -> Button.warning
    Danger -> Button.danger
    Light -> Button.light
    Dark -> Button.dark
    Link -> Button.roleLink 
