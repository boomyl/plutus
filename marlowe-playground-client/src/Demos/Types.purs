module Demos.Types where

import Prelude
import Analytics (class IsEvent, Event)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Projects.Types (Lang)

newtype Demo
  = Demo String

derive instance newtypeDemo :: Newtype Demo _

data Action
  = LoadDemo Lang Demo
  | LoadProject
  | NewProject

defaultEvent :: String -> Event
defaultEvent action = { category: Just "Demos", action, label: Nothing, value: Nothing }

instance isEventAction :: IsEvent Action where
  toEvent (LoadDemo lang _) = Just { category: Just "Demos", action: "LoadDemo", label: Just (show lang), value: Nothing }
  toEvent LoadProject = Just { category: Just "Demos", action: "LoadProject", label: Nothing, value: Nothing }
  toEvent NewProject = Just { category: Just "Demos", action: "NewProject", label: Nothing, value: Nothing }
