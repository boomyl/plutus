module HaskellEditor.View where

import Prelude hiding (div)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Enum (toEnum, upFromIncluding)
import Data.Lens (to, view, (^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Examples.Haskell.Contracts as HE
import Halogen (ClassName(..), ComponentHTML, liftEffect)
import Halogen.Classes (aHorizontal, analysisPanel, closeDrawerArrowIcon, codeEditor, collapsed, footerPanelBg, minimizeIcon)
import Halogen.HTML (HTML, a, button, code_, div, div_, img, option, pre_, section, select, slot, text)
import Halogen.HTML.Events (onClick, onSelectedIndexChange)
import Halogen.HTML.Properties (alt, class_, classes, disabled, src)
import Halogen.HTML.Properties as HTML
import Halogen.Monaco (monacoComponent)
import HaskellEditor.Types (Action(..), State, _compilationResult, _haskellEditorKeybindings, _showBottomPanel)
import Language.Haskell.Interpreter (CompilationError(..), InterpreterError(..), InterpreterResult(..))
import Language.Haskell.Monaco as HM
import LocalStorage as LocalStorage
import Monaco (getModel, setValue) as Monaco
import Network.RemoteData (RemoteData(..), isLoading, isSuccess)
import StaticData as StaticData
import Types (ChildSlots, _haskellEditorSlot, bottomPanelHeight)

render ::
  forall m.
  MonadAff m =>
  State ->
  ComponentHTML Action ChildSlots m
render state =
  div_
    [ section [ class_ (ClassName "code-panel") ]
        [ div [ classes (codeEditor $ state ^. _showBottomPanel) ]
            [ haskellEditor state ]
        ]
    , bottomPanel state
    ]

otherActions :: forall p. State -> HTML p Action
otherActions state =
  div [ classes [ ClassName "group" ] ]
    [ editorOptions state
    , compileButton state
    ]

editorOptions :: forall p. State -> HTML p Action
editorOptions state =
  div [ class_ (ClassName "editor-options") ]
    [ select
        [ HTML.id_ "editor-options"
        , class_ (ClassName "dropdown-header")
        , onSelectedIndexChange (\idx -> ChangeKeyBindings <$> toEnum idx)
        ]
        (map keybindingItem (upFromIncluding bottom))
    ]
  where
  keybindingItem item =
    if state ^. _haskellEditorKeybindings == item then
      option [ class_ (ClassName "selected-item"), HTML.value (show item) ] [ text $ show item ]
    else
      option [ HTML.value (show item) ] [ text $ show item ]

haskellEditor ::
  forall m.
  MonadAff m =>
  State ->
  ComponentHTML Action ChildSlots m
haskellEditor state = slot _haskellEditorSlot unit component unit (Just <<< HandleEditorMessage)
  where
  setup editor =
    liftEffect do
      mContents <- LocalStorage.getItem StaticData.bufferLocalStorageKey
      let
        contents = fromMaybe HE.escrow mContents
      model <- Monaco.getModel editor
      Monaco.setValue model contents

  component = monacoComponent $ HM.settings setup

bottomPanel :: forall p. State -> HTML p Action
bottomPanel state =
  div
    ( [ classes
          ( if showingBottomPanel then
              [ analysisPanel ]
            else
              [ analysisPanel, collapsed ]
          )
      , bottomPanelHeight showingBottomPanel
      ]
    )
    [ div
        [ classes [ footerPanelBg, ClassName "flip-x" ] ]
        [ section [ classes [ ClassName "panel-header", aHorizontal ] ]
            [ div [ classes [ ClassName "panel-sub-header-main", aHorizontal ] ]
                [ div [ class_ (ClassName "minimize-icon-container") ]
                    [ a [ onClick $ const $ Just $ ShowBottomPanel (state ^. _showBottomPanel <<< to not) ]
                        [ img [ classes (minimizeIcon $ state ^. _showBottomPanel), src closeDrawerArrowIcon, alt "close drawer icon" ] ]
                    ]
                , div
                    [ classes ([ ClassName "panel-tab", aHorizontal, ClassName "haskell-buttons" ])
                    ]
                    [ sendResultButton state "Send To Simulator" SendResultToSimulator
                    , sendResultButton state "Send To Blockly" SendResultToBlockly
                    ]
                ]
            ]
        , section
            [ classes [ ClassName "panel-sub-header", aHorizontal, ClassName "panel-contents" ]
            ]
            (resultPane state)
        ]
    ]
  where
  showingBottomPanel = state ^. _showBottomPanel

compileButton :: forall p. State -> HTML p Action
compileButton state = button [ onClick $ const $ Just Compile ] [ text (if state ^. _compilationResult <<< to isLoading then "Compiling..." else "Compile") ]

sendResultButton :: forall p. State -> String -> Action -> HTML p Action
sendResultButton state msg action =
  let
    compilationResult = view _compilationResult state
  in
    case view _compilationResult state of
      Success (Right (InterpreterResult result)) ->
        button
          [ onClick $ const $ Just action
          , disabled (isLoading compilationResult || (not isSuccess) compilationResult)
          ]
          [ text msg ]
      _ -> text ""

resultPane :: forall p. State -> Array (HTML p Action)
resultPane state =
  if state ^. _showBottomPanel then case view _compilationResult state of
    Success (Right (InterpreterResult result)) ->
      [ div [ classes [ ClassName "code-editor", ClassName "expanded", ClassName "code" ] ]
          numberedText
      ]
      -- [ code_
      --     [ pre [ class_ $ ClassName "success-code" ] [ text (unwrap result.result) ]
      --     ]
      -- ]
      where
      numberedText = (code_ <<< Array.singleton <<< text) <$> split (Pattern "\n") result.result
    Success (Left (TimeoutError error)) -> [ text error ]
    Success (Left (CompilationErrors errors)) -> map compilationErrorPane errors
    _ -> [ text "" ]
  else
    [ text "" ]

compilationErrorPane :: forall p. CompilationError -> HTML p Action
compilationErrorPane (RawError error) = div_ [ text error ]

compilationErrorPane (CompilationError error) =
  div
    [ class_ $ ClassName "compilation-error"
    ]
    [ text $ "Line " <> show error.row <> ", Column " <> show error.column <> ":"
    , code_ [ pre_ [ text $ String.joinWith "\n" error.text ] ]
    ]
