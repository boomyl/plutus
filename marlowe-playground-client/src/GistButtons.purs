module GistButtons where

import Prelude hiding (div)
import Auth (AuthRole(..), authStatusAuthRole)
import Data.Either (Either(..))
import Data.Lens (to, view, (^.))
import Data.Maybe (Maybe(..))
import Gist (Gist)
import Gists (GistAction(..), idPublishGist)
import Halogen.Classes (aHorizontal)
import Halogen.HTML (ClassName(..), HTML, a, button, div, span, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes, disabled, href)
import Halogen.SVG (Box(..), Length(..), Linecap(..), RGB(..), circle, clazz, cx, cy, d, fill, height, path, r, strokeLinecap, strokeWidth, svg, viewBox)
import Halogen.SVG as SVG
import Icons (Icon(..), icon)
import Network.RemoteData (RemoteData(..))
import Servant.PureScript.Ajax (AjaxError)
import Types (Action(..), FrontendState, _authStatus, _createGistResult)

authButton :: forall p. FrontendState -> HTML p Action
authButton state =
  let
    authStatus = state ^. (_authStatus <<< to (map (view authStatusAuthRole)))
  in
    case authStatus of
      Failure _ ->
        button
          [ idPublishGist
          ]
          [ text "Failed to login" ]
      Success Anonymous ->
        div [ class_ (ClassName "auth-button-container") ]
          [ a
              [ idPublishGist
              , classes [ ClassName "auth-button" ]
              , href "/api/oauth/github"
              ]
              [ text "Save to GitHub"
              ]
          ]
      Success GithubUser -> gistSection state
      Loading ->
        button
          [ idPublishGist
          , disabled true
          ]
          [ icon Spinner ]
      NotAsked ->
        button
          [ idPublishGist
          , disabled true
          ]
          [ icon Spinner ]

spinner :: forall p a. HTML p a
spinner =
  svg [ clazz (ClassName "spinner"), SVG.width (Px 65), height (Px 65), viewBox (Box { x: 0, y: 0, width: 66, height: 66 }) ]
    [ circle [ clazz (ClassName "path"), fill SVG.None, strokeWidth 6, strokeLinecap Round, cx (Length 33.0), cy (Length 33.0), r (Length 30.0) ] [] ]

arrowDown :: forall p a. HTML p a
arrowDown =
  svg [ clazz (ClassName "arrow-down"), SVG.width (Px 20), height (Px 20), viewBox (Box { x: 0, y: 0, width: 24, height: 24 }) ]
    [ path [ fill (Hex "#832dc4"), d "M19.92,12.08L12,20L4.08,12.08L5.5,10.67L11,16.17V2H13V16.17L18.5,10.66L19.92,12.08M12,20H2V22H22V20H12Z" ] [] ]

arrowUp :: forall p a. HTML p a
arrowUp =
  svg [ clazz (ClassName "arrow-up"), SVG.width (Px 20), height (Px 20), viewBox (Box { x: 0, y: 0, width: 24, height: 24 }) ]
    [ path [ fill (Hex "#832dc4"), d "M4.08,11.92L12,4L19.92,11.92L18.5,13.33L13,7.83V22H11V7.83L5.5,13.33L4.08,11.92M12,4H22V2H2V4H12Z" ] [] ]

errorIcon :: forall p a. HTML p a
errorIcon =
  svg [ clazz (ClassName "error-icon"), SVG.width (Px 20), height (Px 20), viewBox (Box { x: 0, y: 0, width: 24, height: 24 }) ]
    [ path [ fill (Hex "#ff0000"), d "M13,13H11V7H13M12,17.3A1.3,1.3 0 0,1 10.7,16A1.3,1.3 0 0,1 12,14.7A1.3,1.3 0 0,1 13.3,16A1.3,1.3 0 0,1 12,17.3M15.73,3H8.27L3,8.27V15.73L8.27,21H15.73L21,15.73V8.27L15.73,3Z" ] [] ]

gistButtonIcon :: forall p a. HTML p a -> Either String (RemoteData AjaxError Gist) -> HTML p a
gistButtonIcon _ (Left _) = errorIcon

gistButtonIcon _ (Right (Failure _)) = errorIcon

gistButtonIcon arrow (Right (Success _)) = arrow

gistButtonIcon _ (Right Loading) = spinner

gistButtonIcon arrow (Right NotAsked) = arrow

gistSection :: forall p. FrontendState -> HTML p Action
gistSection state =
  div [ classes [ ClassName "save-button-group" ] ]
    [ button
        [ onClick $ const $ Just $ GistAction PublishGist ]
        [ text saveText ]
    , span [ class_ (ClassName "error") ] [ text error ]
    ]
  where
  error = case state ^. _createGistResult of
    (Failure _) -> "Failed to save project"
    _ -> ""
  saveText = case state ^. _createGistResult of
    Loading -> "Saving..."
    _ -> "Save"
