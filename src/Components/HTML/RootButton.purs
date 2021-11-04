module LogicWeb.Components.HTML.RootButton where

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import LogicWeb.Components.Common (css)
import Web.UIEvent.MouseEvent (MouseEvent)

button :: forall w i. Array (HH.HTML w i) -> (MouseEvent -> i) -> HH.HTML w i
button content eventHandler = HH.div
  [ css "text-yukiRed bg-white border-yukiRed border-2 text-lg rounded-md p-2 flex items-center justify-center cursor-pointer hover:bg-yukiRed hover:text-white duration-75"
  , HE.onClick eventHandler]
  content