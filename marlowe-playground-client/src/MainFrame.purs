module MainFrame (mkMainFrame) where

import Auth (_AuthStatus, _GithubUser, authStatusAuthRole)
import Control.Monad.Except (ExceptT(..), lift, runExceptT)
import Control.Monad.Maybe.Extra (hoistMaybe)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Reader (runReaderT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, note)
import Data.Foldable (for_, traverse_)
import Data.Lens (_Right, assign, has, is, preview, previewOn, to, use, view, (^.))
import Data.Lens.Extra (peruse)
import Data.Lens.Index (ix)
import Data.List.NonEmpty as NEL
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Debug.Trace (trace)
import Demos (handleAction) as Demos
import Demos.Types (Action(..), Demo(..)) as Demos
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Gist (Gist, _GistId, gistDescription, gistId)
import GistButtons as GistButtons
import Gists (GistAction(..))
import Gists as Gists
import Halogen (ClassName(..), Component, ComponentHTML, get, liftEffect, query, subscribe, subscribe')
import Halogen as H
import Halogen.ActusBlockly as ActusBlockly
import Halogen.Analytics (handleActionWithAnalyticsTracking)
import Halogen.Blockly (BlocklyMessage(..), blockly)
import Halogen.Blockly as Blockly
import Halogen.Classes (aHorizontal, active, fullHeight, fullWidth, hide, noMargins, spaceLeft, spaceRight, uppercase)
import Halogen.Classes as Classes
import Halogen.Extra (mapSubmodule, renderSubmodule)
import Halogen.HTML (ClassName(ClassName), HTML, a, div, h1, h1_, h2, h2_, header, main, section, slot, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes, href, id_, target)
import Halogen.Monaco (KeyBindings(DefaultBindings))
import Halogen.Monaco as Monaco
import Halogen.Query (HalogenM)
import Halogen.Query.EventSource (affEventSource, emit, eventListenerEventSource)
import Halogen.SVG (GradientUnits(..), Translate(..), d, defs, gradientUnits, linearGradient, offset, path, stop, stopColour, svg, transform, x1, x2, y2)
import Halogen.SVG as SVG
import HaskellEditor as HaskellEditor
import HaskellEditor.Types (_compilationResult)
import HaskellEditor.Types as HE
import Home as Home
import JSEditor as JSEditor
import Language.Haskell.Interpreter (_InterpreterResult)
import Language.Haskell.Monaco as HM
import Language.Javascript.Interpreter as JSI
import LocalStorage as LocalStorage
import Marlowe (SPParams_, getApiGists, getApiGistsByGistId)
import Marlowe as Server
import Marlowe.ActusBlockly as AMB
import Marlowe.Blockly as MB
import Marlowe.Gists (mkNewGist, playgroundFiles)
import Marlowe.Parser (parseContract)
import Network.RemoteData (RemoteData(..), _Success)
import Network.RemoteData as RemoteData
import NewProject (handleAction, render) as NewProject
import NewProject.Types (Action(..), State, _error, _projectName, emptyState) as NewProject
import Prelude (class Functor, Unit, Void, bind, const, discard, eq, flip, identity, map, mempty, negate, otherwise, pure, show, unit, void, ($), (/=), (<$>), (<<<), (<>), (=<<), (==), (>))
import Projects (handleAction) as Projects
import Projects as Project
import Projects.Types (Action(..), State, _projects, emptyState) as Projects
import Projects.Types (Lang(..))
import Router (Route, SubRoute)
import Router as Router
import Routing.Duplex as RD
import Routing.Duplex as RT
import Routing.Hash as Routing
import Servant.PureScript.Ajax (AjaxError, ErrorDescription(..), errorToString, runAjaxError)
import Servant.PureScript.Settings (SPSettings_)
import Simulation as Simulation
import Simulation.State (_result)
import Simulation.Types (_marloweState)
import Simulation.Types as ST
import StaticData (bufferLocalStorageKey, gistIdLocalStorageKey, jsBufferLocalStorageKey, marloweBufferLocalStorageKey, showHomePageLocalStorageKey)
import StaticData as StaticData
import Text.Pretty (pretty)
import Types (Action(..), ChildSlots, FrontendState(FrontendState), JSCompilationState(..), ModalView(..), Query(..), View(..), WebData, _activeJSDemo, _actusBlocklySlot, _authStatus, _blocklySlot, _createGistResult, _gistId, _haskellEditorSlot, _haskellState, _jsCompilationResult, _jsEditorKeybindings, _jsEditorSlot, _loadGistResult, _newProject, _projectName, _projects, _showBottomPanel, _showHomePage, _showModal, _simulationState, _view, _walletSlot)
import Wallet as Wallet
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes (keyup)

initialState :: FrontendState
initialState =
  FrontendState
    { view: Simulation
    , jsCompilationResult: JSNotCompiled
    , blocklyState: Nothing
    , actusBlocklyState: Nothing
    , showBottomPanel: true
    , haskellState: HE.initialState
    , simulationState: ST.mkState
    , jsEditorKeybindings: DefaultBindings
    , activeJSDemo: mempty
    , showHomePage: true
    , projects: Projects.emptyState
    , newProject: NewProject.emptyState
    , authStatus: NotAsked
    , gistId: Nothing
    , createGistResult: NotAsked
    , loadGistResult: Right NotAsked
    , projectName: "Untitled Project"
    , showModal: Nothing
    }

------------------------------------------------------------
mkMainFrame ::
  forall m.
  MonadAff m =>
  SPSettings_ SPParams_ -> Component HTML Query Unit Void m
mkMainFrame settings =
  H.mkComponent
    { initialState: const initialState
    , render: render settings
    , eval:
      H.mkEval
        { handleQuery: handleQuery settings
        , handleAction: handleActionWithAnalyticsTracking (handleAction settings)
        , receive: const Nothing
        , initialize: Just Init
        , finalize: Nothing
        }
    }

toSimulation ::
  forall m a.
  Functor m =>
  HalogenM ST.State ST.Action ChildSlots Void m a -> HalogenM FrontendState Action ChildSlots Void m a
toSimulation = mapSubmodule _simulationState SimulationAction

toHaskellEditor ::
  forall m a.
  Functor m =>
  HalogenM HE.State HE.Action ChildSlots Void m a -> HalogenM FrontendState Action ChildSlots Void m a
toHaskellEditor = mapSubmodule _haskellState HaskellAction

toProjects ::
  forall m a.
  Functor m =>
  HalogenM Projects.State Projects.Action ChildSlots Void m a -> HalogenM FrontendState Action ChildSlots Void m a
toProjects = mapSubmodule _projects ProjectsAction

toNewProject ::
  forall m a.
  Functor m =>
  HalogenM NewProject.State NewProject.Action ChildSlots Void m a -> HalogenM FrontendState Action ChildSlots Void m a
toNewProject = mapSubmodule _newProject NewProjectAction

toDemos ::
  forall m a.
  Functor m =>
  HalogenM FrontendState Demos.Action ChildSlots Void m a -> HalogenM FrontendState Action ChildSlots Void m a
toDemos = mapSubmodule identity DemosAction

handleSubRoute ::
  forall m.
  MonadEffect m =>
  MonadAff m =>
  SPSettings_ SPParams_ ->
  SubRoute -> HalogenM FrontendState Action ChildSlots Void m Unit
handleSubRoute _ Router.Home = selectView HomePage

handleSubRoute _ Router.Simulation = selectView Simulation

handleSubRoute _ Router.HaskellEditor = selectView HaskellEditor

handleSubRoute _ Router.JSEditor = selectView JSEditor

handleSubRoute _ Router.Blockly = selectView BlocklyEditor

handleSubRoute _ Router.ActusBlocklyEditor = selectView ActusBlocklyEditor

handleSubRoute _ Router.Wallets = selectView WalletEmulator

handleRoute ::
  forall m.
  MonadEffect m =>
  MonadAff m =>
  SPSettings_ SPParams_ ->
  Route -> HalogenM FrontendState Action ChildSlots Void m Unit
handleRoute settings { gistId: (Just gistId), subroute } = do
  handleAction settings (GistAction (SetGistUrl (unwrap gistId)))
  handleAction settings (GistAction LoadGist)
  handleSubRoute settings subroute

handleRoute settings { subroute } = handleSubRoute settings subroute

handleQuery ::
  forall m a.
  Functor m =>
  MonadEffect m =>
  MonadAff m =>
  SPSettings_ SPParams_ ->
  Query a ->
  HalogenM FrontendState Action ChildSlots Void m (Maybe a)
handleQuery settings (ChangeRoute route next) = do
  handleRoute settings route
  pure $ Just next

handleAction ::
  forall m.
  MonadAff m =>
  SPSettings_ SPParams_ ->
  Action ->
  HalogenM FrontendState Action ChildSlots Void m Unit
handleAction settings Init = do
  let
    isTrue = (/=) (Just "false")
  showHome <- liftEffect $ isTrue <$> LocalStorage.getItem showHomePageLocalStorageKey
  let
    subroute = if showHome then Router.Home else Router.Simulation
  assign _showHomePage showHome
  hash <- liftEffect Routing.getHash
  case (RD.parse Router.route) hash of
    Right { subroute: Router.Home, gistId } -> handleRoute settings { subroute, gistId }
    Right route -> handleRoute settings route
    Left _ -> handleRoute settings { subroute, gistId: Nothing }
  document <- liftEffect $ Web.document =<< Web.window
  subscribe' \sid ->
    eventListenerEventSource keyup (toEventTarget document) (map (HandleKey sid) <<< KE.fromEvent)
  toSimulation $ Simulation.handleAction settings ST.Init
  checkAuthStatus settings

handleAction settings (HandleKey sid ev)
  | KE.key ev == "Escape" = trace "Handle Escape" \_ -> assign _showModal Nothing
  | otherwise = pure unit

handleAction settings (ShowHomePageInFuture b) = do
  liftEffect $ LocalStorage.setItem showHomePageLocalStorageKey (show b)
  assign _showHomePage b

handleAction s (HaskellAction action) = do
  currentState <- get
  toHaskellEditor (HaskellEditor.handleAction s action)
  case action of
    HE.SendResultToSimulator -> do
      mContract <-
        peruse
          ( _haskellState
              <<< _compilationResult
              <<< _Success
              <<< _Right
              <<< _InterpreterResult
              <<< _result
          )
      let
        contract = case mContract of
          Just unformatted -> case parseContract unformatted of
            Right pcon -> show $ pretty pcon
            Left _ -> unformatted
          _ -> ""
      selectView Simulation
      void $ toSimulation
        $ do
            Simulation.handleAction s (ST.SetEditorText contract)
            Simulation.handleAction s ST.ResetContract
    HE.SendResultToBlockly -> selectView BlocklyEditor
    _ -> pure unit

handleAction s (SimulationAction action) = do
  currentState <- get
  toSimulation (Simulation.handleAction s action)
  case action of
    ST.SetBlocklyCode -> do
      mSource <- Simulation.editorGetValue
      for_ mSource \source -> void $ query _blocklySlot unit (Blockly.SetCode source unit)
      selectView BlocklyEditor
    ST.Save -> pure unit
    _ -> pure unit

handleAction _ (HandleWalletMessage Wallet.SendContractToWallet) = do
  contract <- toSimulation $ Simulation.getCurrentContract
  void $ query _walletSlot unit (Wallet.LoadContract contract unit)

handleAction _ (JSHandleEditorMessage (Monaco.TextChanged text)) = do
  liftEffect $ LocalStorage.setItem jsBufferLocalStorageKey text
  assign _activeJSDemo ""

handleAction _ (JSSelectEditorKeyBindings bindings) = do
  assign _jsEditorKeybindings bindings
  void $ query _jsEditorSlot unit (Monaco.SetKeyBindings bindings unit)

handleAction _ CompileJSProgram = do
  maybeModel <- query _jsEditorSlot unit (Monaco.GetModel identity)
  case maybeModel of
    Nothing -> pure unit
    Just model -> do
      assign _jsCompilationResult JSCompiling
      void $ subscribe
        $ affEventSource
            ( \emitter -> do
                res <- JSI.eval model
                emit emitter (CompiledJSProgram res)
                pure mempty
            )
      pure unit

handleAction _ (CompiledJSProgram res) = do
  assign _jsCompilationResult (either JSCompilationError JSCompiledSuccessfully res)
  assign _showBottomPanel true

handleAction _ (LoadJSScript key) = do
  case Map.lookup key StaticData.demoFilesJS of
    Nothing -> pure unit
    Just contents -> do
      void $ query _jsEditorSlot unit (Monaco.SetText contents unit)
      assign _activeJSDemo key

handleAction s SendResultJSToSimulator = do
  mContract <- use _jsCompilationResult
  case mContract of
    JSNotCompiled -> pure unit
    JSCompiling -> pure unit
    JSCompilationError err -> pure unit
    JSCompiledSuccessfully (JSI.InterpreterResult { result: contract }) -> do
      selectView Simulation
      void $ toSimulation
        $ do
            Simulation.handleAction s (ST.SetEditorText (show $ pretty contract))
            Simulation.handleAction s ST.ResetContract

handleAction _ (ChangeView view) = selectView view

handleAction _ (ShowBottomPanel val) = do
  assign _showBottomPanel val
  pure unit

handleAction s (HandleBlocklyMessage (CurrentCode code)) = do
  hasStarted <- use (_simulationState <<< _marloweState <<< to (\states -> (NEL.length states) > 1))
  if hasStarted then
    void $ query _blocklySlot unit (Blockly.SetError "You can't send new code to a running simulation. Please go to the Simulation tab and click \"reset\" first" unit)
  else do
    selectView Simulation
    void $ toSimulation $ Simulation.handleAction s (ST.SetEditorText code)

handleAction _ (HandleActusBlocklyMessage ActusBlockly.Initialized) = pure unit

handleAction s (HandleActusBlocklyMessage (ActusBlockly.CurrentTerms flavour terms)) = do
  hasStarted <- use (_simulationState <<< _marloweState <<< to (\states -> (NEL.length states) > 1))
  let
    parsedTermsEither = AMB.parseActusJsonCode terms
  if hasStarted then
    void $ query _actusBlocklySlot unit (ActusBlockly.SetError "You can't send new code to a running simulation. Please go to the Simulation tab and click \"reset\" first" unit)
  else case parsedTermsEither of
    Left e -> void $ query _actusBlocklySlot unit (ActusBlockly.SetError ("Couldn't parse contract-terms - " <> (show e)) unit)
    Right parsedTerms -> do
      result <- case flavour of
        ActusBlockly.FS -> runAjax $ flip runReaderT s $ (Server.postApiActusGenerate parsedTerms)
        ActusBlockly.F -> runAjax $ flip runReaderT s $ (Server.postApiActusGeneratestatic parsedTerms)
      case result of
        Success contractAST -> do
          selectView Simulation
          void $ toSimulation $ Simulation.handleAction s (ST.SetEditorText contractAST)
        Failure e -> void $ query _actusBlocklySlot unit (ActusBlockly.SetError ("Server error! " <> (showErrorDescription (runAjaxError e).description)) unit)
        _ -> void $ query _actusBlocklySlot unit (ActusBlockly.SetError "Unknown server error!" unit)

handleAction s (ProjectsAction action@(Projects.LoadProject lang gistId)) = do
  res <-
    runExceptT
      $ do
          gist <- flip runReaderT s $ getApiGistsByGistId gistId
          lift $ loadGist gist
  case res of
    Right _ -> pure unit
    Left error -> assign (_projects <<< Projects._projects) (Failure "Failed to load gist")
  toProjects $ Projects.handleAction s action
  traverse_ selectView $ selectLanguageView lang

handleAction s (ProjectsAction action) = toProjects $ Projects.handleAction s action

handleAction s (NewProjectAction action@(NewProject.CreateProject lang)) = do
  -- we want to keep the current poject details in case creating a new project fails
  currentProject <- use _projectName
  currentGistId <- use _gistId
  description <- use (_newProject <<< NewProject._projectName)
  assign _projectName description
  assign _gistId Nothing
  handleGistAction s PublishGist
  res <- peruse (_createGistResult <<< _Success)
  case res of
    Just gist -> do
      liftEffect $ LocalStorage.setItem gistIdLocalStorageKey (gist ^. (gistId <<< _GistId))
      traverse_ selectView $ selectLanguageView lang
    Nothing -> do
      assign (_newProject <<< NewProject._error) (Just "Could not create new project")
      assign _projectName currentProject
      assign _gistId currentGistId
  toNewProject $ NewProject.handleAction s action

handleAction s (NewProjectAction action) = toNewProject $ NewProject.handleAction s action

handleAction s (DemosAction action@Demos.LoadProject) = pure unit

handleAction s (DemosAction action@Demos.NewProject) = pure unit

handleAction s (DemosAction action@(Demos.LoadDemo lang (Demos.Demo key))) = do
  for_ (Map.lookup key StaticData.demoFiles) \contents -> HaskellEditor.editorSetValue contents
  for_ (Map.lookup key StaticData.demoFilesJS) \contents -> void $ query _jsEditorSlot unit (Monaco.SetText contents unit)
  for_ (preview (ix key) (Map.fromFoldable StaticData.marloweContracts)) \contents -> do
    Simulation.editorSetValue contents
    void $ query _blocklySlot unit (Blockly.SetCode contents unit)
  toDemos $ Demos.handleAction s action
  traverse_ selectView $ selectLanguageView lang

handleAction settings CheckAuthStatus = do
  checkAuthStatus settings

handleAction settings (GistAction subEvent) = handleGistAction settings subEvent

handleAction settings (OpenModal OpenProject) = trace "OpenProject" \_ -> do
  assign _showModal $ Just OpenProject
  toProjects $ Projects.handleAction settings Projects.LoadProjects
  -- assign (_projects <<< Projects._projects) Loading
  -- resp <- flip runReaderT settings $ runExceptT getApiGists
  -- assign (_projects <<< Projects._projects) $ lmap errorToString $ RemoteData.fromEither resp

handleAction _ (OpenModal modal) = assign _showModal $ Just modal

handleAction _ CloseModal = trace "CloseModal" \_ -> assign _showModal Nothing

selectLanguageView :: Lang -> Maybe View
selectLanguageView Haskell = Just HaskellEditor

selectLanguageView Marlowe = Just Simulation

selectLanguageView Blockly = Just BlocklyEditor

selectLanguageView Javascript = Nothing

----------
showErrorDescription :: ErrorDescription -> String
showErrorDescription (DecodingError err@"(\"Unexpected token E in JSON at position 0\" : Nil)") = "BadResponse"

showErrorDescription (DecodingError err) = "DecodingError: " <> err

showErrorDescription (ResponseFormatError err) = "ResponseFormatError: " <> err

showErrorDescription (ConnectionError err) = "ConnectionError: " <> err

runAjax ::
  forall m a.
  ExceptT AjaxError (HalogenM FrontendState Action ChildSlots Void m) a ->
  HalogenM FrontendState Action ChildSlots Void m (WebData a)
runAjax action = RemoteData.fromEither <$> runExceptT action

------------------------------------------------------------
checkAuthStatus :: forall m. MonadAff m => SPSettings_ SPParams_ -> HalogenM FrontendState Action ChildSlots Void m Unit
checkAuthStatus settings = do
  assign _authStatus Loading
  authResult <- runAjax $ runReaderT Server.getApiOauthStatus settings
  assign _authStatus authResult

handleGistAction ::
  forall m.
  MonadAff m =>
  MonadEffect m =>
  SPSettings_ SPParams_ -> GistAction -> HalogenM FrontendState Action ChildSlots Void m Unit
handleGistAction settings PublishGist = do
  description <- use _projectName
  let
    -- playground is a meta-data file that we currently just use as a tag to check if a gist is a marlowe playground gist
    playground = "{}"
  marlowe <- toSimulation Simulation.editorGetValue
  haskell <- HaskellEditor.editorGetValue
  blockly <- query _blocklySlot unit $ H.request Blockly.GetWorkspace
  javascript <- query _jsEditorSlot unit $ H.request Monaco.GetText
  actus <- query _actusBlocklySlot unit $ H.request ActusBlockly.GetWorkspace
  let
    files = { playground, marlowe, haskell, blockly, javascript, actus }

    newGist = mkNewGist description files
  void
    $ runMaybeT do
        mGist <- use _gistId
        assign _createGistResult Loading
        newResult <-
          lift
            $ case mGist of
                Nothing -> runAjax $ flip runReaderT settings $ Server.postApiGists newGist
                Just gistId -> runAjax $ flip runReaderT settings $ Server.patchApiGistsByGistId newGist gistId
        assign _createGistResult newResult
        gistId <- hoistMaybe $ preview (_Success <<< gistId) newResult
        assign _gistId (Just gistId)
        assign _loadGistResult (Right NotAsked)

handleGistAction _ (SetGistUrl url) = do
  case Gists.parseGistUrl url of
    Right newGistUrl -> do
      assign _createGistResult NotAsked
      assign _loadGistResult (Right NotAsked)
      assign _gistId (Just newGistUrl)
    Left _ -> pure unit

handleGistAction settings LoadGist = do
  res <-
    runExceptT
      $ do
          eGistId <- ExceptT $ note "Gist Id not set." <$> use _gistId
          assign _loadGistResult $ Right Loading
          aGist <- lift $ runAjax $ flip runReaderT settings $ Server.getApiGistsByGistId eGistId
          assign _loadGistResult $ Right aGist
          gist <- ExceptT $ pure $ toEither (Left "Gist not loaded.") $ lmap errorToString aGist
          lift $ loadGist gist
          pure aGist
  assign _loadGistResult res
  where
  toEither :: forall e a. Either e a -> RemoteData e a -> Either e a
  toEither _ (Success a) = Right a

  toEither _ (Failure e) = Left e

  toEither x Loading = x

  toEither x NotAsked = x

loadGist ::
  forall m.
  MonadAff m =>
  MonadEffect m =>
  Gist ->
  HalogenM FrontendState Action ChildSlots Void m Unit
loadGist gist = do
  let
    { marlowe
    , haskell
    , blockly
    , javascript
    , actus
    } = playgroundFiles gist

    description = view gistDescription gist

    gistId' = preview gistId gist
  for_ haskell \s -> liftEffect $ LocalStorage.setItem bufferLocalStorageKey s
  for_ javascript \s -> liftEffect $ LocalStorage.setItem jsBufferLocalStorageKey s
  for_ marlowe \s -> liftEffect $ LocalStorage.setItem marloweBufferLocalStorageKey s
  assign _gistId gistId'
  assign _projectName description
  toSimulation $ Simulation.editorSetValue $ fromMaybe mempty marlowe
  HaskellEditor.editorSetValue (fromMaybe mempty haskell)
  for_ blockly \xml -> query _blocklySlot unit (Blockly.LoadWorkspace xml unit)
  void $ query _jsEditorSlot unit (Monaco.SetText (fromMaybe mempty javascript) unit)
  for_ actus \xml -> query _actusBlocklySlot unit (ActusBlockly.LoadWorkspace xml unit)

------------------------------------------------------------
selectView ::
  forall m action message.
  MonadEffect m =>
  View -> HalogenM FrontendState action ChildSlots message m Unit
selectView view = do
  let
    subroute = case view of
      HomePage -> Router.Home
      Simulation -> Router.Simulation
      HaskellEditor -> Router.HaskellEditor
      JSEditor -> Router.JSEditor
      BlocklyEditor -> Router.Blockly
      WalletEmulator -> Router.Wallets
      ActusBlocklyEditor -> Router.ActusBlocklyEditor
  liftEffect $ Routing.setHash (RT.print Router.route { subroute, gistId: Nothing })
  assign _view view
  case view of
    HomePage -> pure unit
    Simulation -> do
      Simulation.editorResize
      Simulation.editorSetTheme
    HaskellEditor -> do
      HaskellEditor.editorResize
      void $ query _haskellEditorSlot unit (Monaco.SetTheme HM.daylightTheme.name unit)
    JSEditor -> do
      void $ query _jsEditorSlot unit (Monaco.Resize unit)
      void $ query _jsEditorSlot unit (Monaco.SetTheme HM.daylightTheme.name unit)
    BlocklyEditor -> void $ query _blocklySlot unit (Blockly.Resize unit)
    WalletEmulator -> pure unit
    ActusBlocklyEditor -> void $ query _actusBlocklySlot unit (ActusBlockly.Resize unit)

render ::
  forall m.
  MonadAff m =>
  SPSettings_ SPParams_ ->
  FrontendState ->
  ComponentHTML Action ChildSlots m
render settings state =
  div [ class_ (ClassName "site-wrap") ]
    ( [ header [ classes [ noMargins, aHorizontal ] ]
          [ div [ classes [ aHorizontal, fullWidth ] ]
              [ div [ classes [ ClassName "group", aHorizontal, ClassName "marlowe-title-group" ] ]
                  [ div [ class_ (ClassName "marlowe-logo") ]
                      [ svg [ SVG.width (SVG.Length 50.0), SVG.height (SVG.Length 41.628), SVG.viewBox (SVG.Box { x: 0, y: 0, width: 60, height: 42 }) ]
                          [ defs []
                              [ linearGradient [ id_ "marlowe__linear-gradient", x1 (SVG.Length 0.5), x2 (SVG.Length 0.5), y2 (SVG.Length 1.0), gradientUnits ObjectBoundingBox ]
                                  [ stop [ offset (SVG.Length 0.221), stopColour "#832dc4" ] []
                                  , stop [ offset (SVG.Length 0.377), stopColour "#5e35b8" ] []
                                  , stop [ offset (SVG.Length 0.543), stopColour "#3f3dad" ] []
                                  , stop [ offset (SVG.Length 0.704), stopColour "#2942a6" ] []
                                  , stop [ offset (SVG.Length 0.857), stopColour "#1c45a2" ] []
                                  , stop [ offset (SVG.Length 0.994), stopColour "#1746a0" ] []
                                  ]
                              ]
                          , path
                              [ id_ "prefix__marlowe-logo"
                              , d "M90.464 35.544c1.02 0 2.232.024 2.736.072V30.4a42.042 42.042 0 00-30.06 10.124c-8.88-7.68-20.784-10.992-29.916-9.96v4.884c.516-.036 1.308-.06 2.208-.06h.048l.156-.012.2.012a19.663 19.663 0 012.264.112h.1c12.324 1.488 21.984 7.212 28.7 17.556a236 236 0 00-3.792 6.3c-.756-1.236-2.832-5.04-3.672-6.444a44.98 44.98 0 012.028-3.06c-1.284-1.26-2.484-2.4-3.732-3.588-.9 1.116-1.62 1.992-2.412 2.964-3.36-2.28-6.576-4.476-10.392-5.628A29.291 29.291 0 0033.2 42.228v29.688h4.98V47.424c5.028.876 10.332 2.736 14.472 6.672a46.733 46.733 0 00-3.9 17.832h5.172a34.82 34.82 0 012.628-13.644 43.568 43.568 0 013.24 7.884 44.62 44.62 0 01.864 5.736h2.3v-8.268h.072a.77.77 0 11.84-.768.759.759 0 01-.684.768h.072V71.9h-.3l.072.012h.228V71.9h2.4a24.792 24.792 0 014.128-13.728 42.589 42.589 0 012.7 13.74h5.296c0-5.088-1.992-14.6-4.092-18.552a22.176 22.176 0 0114.244-5.616c0 4-.012 8 0 12.012.012 4.032-.084 8.076.072 12.144h5.2V42.144a35.632 35.632 0 00-12.012 1.512 33.507 33.507 0 00-10.468 5.664c-1.092-1.9-2.316-3.432-3.564-5.244a37.471 37.471 0 0120.892-8.46c.504-.048 1.392-.072 2.412-.072z"
                              , transform (Translate { x: (negate 33.2), y: (negate 30.301) })
                              ]
                              []
                          ]
                      ]
                  , h2 [ classes [ spaceLeft, uppercase, spaceRight ] ] [ text "Marlowe Playground" ]
                  ]
              , h1 [ classes [ ClassName "project-title" ] ] [ text (state ^. _projectName) ]
              , a [ href "./tutorial/index.html", target "_blank", classes [ ClassName "external-links" ] ] [ text "Tutorial" ]
              ]
          ]
      , main []
          [ menuBar state
          , globalActions (state ^. _view)
          , section [ id_ "main-panel" ]
              [ tabContents HomePage [ Home.render state ]
              , tabContents Simulation
                  [ div []
                      [ renderSubmodule _simulationState SimulationAction Simulation.render state ]
                  ]
              , tabContents HaskellEditor
                  [ div []
                      [ renderSubmodule _haskellState HaskellAction HaskellEditor.render state ]
                  , renderSubmodule _haskellState HaskellAction HaskellEditor.bottomPanel state
                  ]
              , tabContents JSEditor
                  [ div [] (JSEditor.render state)
                  , JSEditor.bottomPanel state
                  ]
              , tabContents BlocklyEditor
                  [ div []
                      [ slot _blocklySlot unit (blockly MB.rootBlockName MB.blockDefinitions) unit (Just <<< HandleBlocklyMessage)
                      , MB.toolbox
                      , MB.workspaceBlocks
                      ]
                  ]
              , tabContents ActusBlocklyEditor
                  [ div []
                      [ slot _actusBlocklySlot unit (ActusBlockly.blockly AMB.rootBlockName AMB.blockDefinitions) unit (Just <<< HandleActusBlocklyMessage)
                      , AMB.toolbox
                      , AMB.workspaceBlocks
                      ]
                  ]
              , tabContents WalletEmulator
                  [ div [ classes [ ClassName "full-height" ] ]
                      [ slot _walletSlot unit Wallet.mkComponent unit (Just <<< HandleWalletMessage) ]
                  ]
              ]
          ]
      ]
        <> modal (state ^. _showModal)
    )
  where
  isActiveView activeView = state ^. _view <<< to (eq activeView)

  isActiveTab activeView = if isActiveView activeView then [ active ] else []

  tabContents activeView contents = if isActiveView activeView then div [ classes [ fullHeight, Classes.scroll ] ] contents else div [ classes [ hide ] ] contents

  globalActions HaskellEditor =
    div [ class_ (ClassName "global-actions") ]
      [ div [ classes [ ClassName "group" ] ]
          [ renderSubmodule _haskellState HaskellAction HaskellEditor.editorOptions state
          , renderSubmodule _haskellState HaskellAction HaskellEditor.compileButton state
          ]
      ]

  globalActions Simulation =
    div [ class_ (ClassName "global-actions") ]
      [ div [ classes [ ClassName "group" ] ]
          [ renderSubmodule _simulationState SimulationAction Simulation.editorOptions state
          , renderSubmodule _simulationState SimulationAction Simulation.sendToBlocklyButton state
          ]
      ]

  globalActions _ = text ""

  modal Nothing = []

  modal (Just view) =
    [ div [ classes [ ClassName "modal" ] ]
        [ div [ classes [ ClassName "modal-container" ], onClick $ const $ Just CloseModal ]
            [ div [ classes [ ClassName "modal-content" ] ]
                [ a [ class_ (ClassName "close"), onClick $ const $ Just CloseModal ] [ text "x" ]
                , modalContent view
                ]
            ]
        ]
    ]

  modalContent NewProject = renderSubmodule _newProject NewProjectAction NewProject.render state

  modalContent OpenProject = renderSubmodule _projects ProjectsAction Project.render state

  modalContent OpenDemo = text "OpenDemo"

  modalContent RenameProject = text "RenameProject"

  modalContent SaveProjectAs = text "SaveProjectAs"

  modalContent GithubLogin = text "GithubLogin"

menuBar :: forall p. FrontendState -> HTML p Action
menuBar state =
  div [ classes [ ClassName "menu-bar" ] ]
    ( [ openModal NewProject "New Project"
      , openModal OpenProject "Open Project"
      , openModal OpenDemo "Open Demo"
      , openModal RenameProject "Rename"
      ]
        <> save (previewOn state (_authStatus <<< _Success <<< authStatusAuthRole <<< _GithubUser))
        <> saveAs (previewOn state (_authStatus <<< _Success <<< authStatusAuthRole <<< _GithubUser))
    )
  where
  openModal modal name = a [ onClick $ const $ Just $ OpenModal modal ] [ text name ]

  save Nothing = [ openModal GithubLogin "Save" ]

  save _ = [ a [ onClick $ const $ Just $ GistAction PublishGist ] [ text "Save" ] ]

  saveAs Nothing = [ openModal GithubLogin "Save As" ]

  saveAs _ = [ openModal SaveProjectAs "Save As" ]
