module Demos where

import Prelude hiding (div)

import Data.Maybe (Maybe(..))
import Demos.Types (Action(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..), ComponentHTML, HalogenM)
import Halogen.HTML (button, div, hr_, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes)
import Marlowe (SPParams_)
import Servant.PureScript.Settings (SPSettings_)
import Types (ChildSlots)

handleAction ::
  forall m state.
  MonadAff m =>
  SPSettings_ SPParams_ ->
  Action -> HalogenM state Action ChildSlots Void m Unit
handleAction _ _ = pure unit

render ::
  forall m state.
  MonadAff m =>
  state ->
  ComponentHTML Action ChildSlots m
render state =
  div [ classes [ ClassName "projects-container" ] ]
    [ button [ onClick $ const $ Just NewProject ] [ text "Create a new project" ]
    , button [ onClick $ const $ Just LoadProject ] [ text "Load an existing project" ]
    , hr_
    ]

demos :: Array String
demos = [ "Escrow", "ZeroCouponBond", "CouponBondGuaranteed", "Swap" ]
