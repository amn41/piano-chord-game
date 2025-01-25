module Piano.Page
  ( Action(..)
  , Query(..)
  , Slot
  , State
  , component
  )
  where

import Prelude
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Now
import Effect.Random (randomInt)
import Halogen.Aff as HA
import Halogen as H
import Halogen (liftEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (ClassName(..))
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY)
import Web.HTML.HTMLElement (offsetTop, offsetLeft)
import DOM.HTML.Indexed.StepValue (StepValue(..))
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event as E
import Graphics.Canvas
  ( Context2D
  , CanvasElement
  , clearRect
  , getCanvasElementById
  , getContext2D
  )
import Graphics.Drawing (render) as Drawing
import Data.Map (lookup, values, size, pop, keys)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust)
import Data.Array as Array
import Data.Array (cons, filter, length, index, fromFoldable)
import Data.List (toUnfoldable)
import Data.Tuple (Tuple(..))
import Data.DateTime
import Data.Enum
import Data.Time.Duration (Seconds(..))
import Data.Foldable (foldl)
import Partial.Unsafe (unsafePartial)
import Data.Int (toNumber, fromString)
import Piano.Graphics (canvasHeight, canvasWidth, displayChord, fingeredKey)
import Piano.Types (ChordShape, Fingering, unfingered)
import Piano.Audio (playChord)
import Piano.Chords (ChordMap, chordMap)
import Common.Types (ExportFormat(..), CanvasPosition, Percentage)
import Common.Export (exportAs, scaleCanvas, toMimeType)
import Common.Utils (contains, safeName, jsonFileInputCtx)
import Audio.SoundFont (Instrument, loadRemoteSoundFonts)
import Data.Midi.Instrument (InstrumentName(AcousticGrandPiano))
import JS.FileIO (saveTextFile)
import Halogen.FileInputComponent as FIC
import Data.Validation.Semigroup (validation)
import Type.Proxy (Proxy(..))

type Slot = H.Slot Query Void

-- import Debug.Trace (spy)

type State =
  { mGraphicsContext :: Maybe Context2D
  , mCanvas :: Maybe CanvasElement
  , canvasPosition :: CanvasPosition
  , chordShape :: ChordShape
  , exportScale :: Percentage
  , instruments :: Array Instrument
  , errorText :: String
  , showChordName :: Boolean
  }

data Action
  = Init
  | EditFingering Int Int
  | GetChordName String
  | PlayChord
  | SetRandomChord
  | RevealChordName

data Query a
  = GetCanvasOffset a
  | LoadInstruments a
  | DisplayFingering a

type ChildSlots =
  (loadfile :: FIC.Slot Unit)


component :: ∀ i o m. MonadAff m => H.Component Query i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        , finalize = Nothing
        }
    }
  where

  initialChordShape :: ChordShape
  initialChordShape =
    case lookup "C" chordMap of
      Just cMajor -> { name: "C", fingering: cMajor }
      Nothing -> { name: "silent", fingering: unfingered }

  initialState :: i -> State
  initialState _ =
    { -- mAudioContext : Nothing
      mGraphicsContext: Nothing
    , mCanvas: Nothing
    , canvasPosition: { left: 0.0, top: 0.0 }
    , chordShape: initialChordShape
    , exportScale: 100
    , instruments: []
    , errorText: ""
    , showChordName: false
    }

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
      HH.div_
        [ HH.div  -- New title container
            [ HP.class_ (ClassName "title-container") ]
            [ HH.h1
                [ HP.classes [ ClassName "center", ClassName "main-title" ] ]
                [ HH.text "Spot That Chord" ]
            ]
        , HH.div
            [ HP.class_ (ClassName "canvas-container") ]
            [ HH.canvas
                [ HP.id "canvas"
                , HE.onClick canvasClickHandler
                , HP.height canvasHeight
                , HP.width canvasWidth
                ]
            ]
        , renderButtonGroup state
        , HH.text state.errorText
        ]    

  renderChordNameOrRevealButton :: State -> H.ComponentHTML Action ChildSlots m
  renderChordNameOrRevealButton state =
    if state.showChordName
      then renderChordName state
      else renderRevealChordButton state


  renderChordName :: State -> H.ComponentHTML Action ChildSlots m
  renderChordName state =
    HH.div
      [ HP.class_ $ ClassName "chord-name-display" ]
      [ HH.text $ "Chord: " <> state.chordShape.name ]


  renderButtonGroup :: State -> H.ComponentHTML Action ChildSlots m
  renderButtonGroup state =
    HH.div [ HP.class_ (ClassName "button-group") ]
      [ renderRevealChordButton state
      , renderPlayButton state
      , renderNewChordButton state
      ]

  renderRevealChordButton :: State -> H.ComponentHTML Action ChildSlots m
  renderRevealChordButton state =
    HH.button 
      [ HE.onClick \_ -> RevealChordName
      , HP.class_ $ ClassName "hoverable"
      , HP.enabled true
      ]  
      [ HH.i [ HP.classes [ ClassName "fas", ClassName "fa-eye", ClassName "icon" ] ] []
      , HH.text "Reveal Chord"
      ]

  renderPlayButton :: State -> H.ComponentHTML Action ChildSlots m
  renderPlayButton state =
    let
      enabled =
        (length state.instruments > 0) &&
          (length state.chordShape.fingering > 0)
      className =
        if enabled then "hoverable" else "unhoverable"
    in
      HH.button
          [ HE.onClick \_ -> PlayChord
          , HP.class_ $ ClassName className
          , HP.enabled enabled
          ]
          [ HH.i [ HP.classes [ ClassName "fas", ClassName "fa-music", ClassName "icon" ] ] []
          , HH.text "Hear Chord"
          ]

  renderNewChordButton :: State -> H.ComponentHTML Action ChildSlots m
  renderNewChordButton state =
    let
      enabled =
        (length state.instruments > 0) &&
          (length state.chordShape.fingering > 0)
      className =
        if enabled then "hoverable" else "unhoverable"
    in
      HH.button
            [ HE.onClick \_ -> SetRandomChord
            , HP.class_ $ ClassName className
            , HP.enabled enabled
            ]
            [ HH.i [ HP.classes [ ClassName "fas", ClassName "fa-redo", ClassName "icon" ] ] []
            , HH.text "New Chord"
            ]

  handleAction ∷ Action → H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    Init -> do
      -- state <- H.get
      mCanvas <- H.liftEffect $ getCanvasElementById "canvas"
      let
        canvas = unsafePartial (fromJust mCanvas)
      -- audioCtx = unsafePartial (fromJust state.mAudioContext)
      graphicsCtx <- H.liftEffect $ getContext2D canvas
      -- _ <- H.liftEffect $ Drawing.render graphicsCtx chordDisplay
      _ <- H.modify
        ( \st -> st
            { mGraphicsContext = Just graphicsCtx
            , mCanvas = mCanvas
            }
        )
      _ <- handleQuery (GetCanvasOffset unit)
      _ <- handleQuery (DisplayFingering unit)
      _ <- handleQuery (LoadInstruments unit)
      pure unit
    EditFingering cx cy -> do
      state <- H.get
      let
        x = toNumber cx - state.canvasPosition.left
        y = toNumber cy - state.canvasPosition.top
        mKey = fingeredKey { x, y }
      if (isJust mKey) then do
        let
          {-}
          foo = spy "X:" x
          bar = spy "Y:" y
          -}
          key = unsafePartial (fromJust mKey)
          {-}
          foo = spy "string:" fstring.stringNumber
          bar = spy "fret:" fstring.fretNumber
          -}
          newFingering = alterFingering key state.chordShape.fingering
          newChordShape = state.chordShape { fingering = newFingering }
        _ <- H.modify (\st -> st { chordShape = newChordShape, errorText = "" })
        _ <- handleQuery (DisplayFingering unit)
        pure unit
      else do
        pure unit
    GetChordName name -> do
      state <- H.get
      let
        newShape = state.chordShape { name = name }
        newState = state { chordShape = newShape, errorText = "" }
      _ <- H.put newState
      _ <- handleQuery (DisplayFingering unit)
      pure unit
    PlayChord -> do
      state <- H.get
      H.liftEffect $ playChord state.chordShape.fingering state.instruments
    SetRandomChord -> do
      state <- H.get    
      let chordNames = fromFoldable (keys chordMap) :: Array String
      let chordsLength = size chordMap :: Int
      randomIndex <- liftEffect (randomInt 0 (chordsLength - 1))
      let chosenChordName = index chordNames randomIndex
      case chosenChordName of
        Just name -> do
          let chosenFingering = lookup name chordMap
          case chosenFingering of
            Just (fingering) -> do
              let newChordShape = state.chordShape { name = name, fingering = fingering }
              _ <- H.modify (\st -> st { chordShape = newChordShape, errorText = "" , showChordName = false })
              _ <- handleQuery (DisplayFingering unit)              
              pure unit
            Nothing -> pure unit 
        Nothing -> pure unit      
    RevealChordName -> do
      _ <- H.modify_ \s -> s { showChordName = true }
      pure unit


  handleQuery :: ∀ a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery = case _ of
    -- get the coordinates of the upper left hand corner of the canvas we've
    -- just built.  We need this to find accurate mouse click references relative
    -- to the canvas itsef (not the entire screen). I think it is OK to use DOM
    -- here because we only call this once immediately after initialising the canvas
    GetCanvasOffset next -> do
      mCanvasElement <- H.liftAff $ HA.selectElement (QuerySelector "#canvas")
      let
        canvasElement = unsafePartial (fromJust mCanvasElement)
      left <- H.liftEffect $ offsetLeft canvasElement
      top <- H.liftEffect $ offsetTop canvasElement
      {-}
      let
        foo = spy "Left:" left
        bar = spy "Top:" top
      -}
      _ <- H.modify (\st -> st { canvasPosition = { left, top } })
      pure (Just next)
    LoadInstruments next -> do
      instruments <- H.liftAff $ loadRemoteSoundFonts [ AcousticGrandPiano ]
      _ <- H.modify (\st -> st { instruments = instruments })
      pure (Just next)
    DisplayFingering next -> do
      state <- H.get
      let
        graphicsCtx = unsafePartial (fromJust state.mGraphicsContext)

      _ <- H.liftEffect do
        clearCanvas state
        Drawing.render graphicsCtx
          $ displayChord state.chordShape
      pure (Just next)

  canvasClickHandler :: MouseEvent -> Action
  canvasClickHandler me =
    EditFingering (clientX me) (clientY me)

  clearCanvas :: State -> Effect Unit
  clearCanvas state = do
    let
      graphicsContext = unsafePartial (fromJust state.mGraphicsContext)
    clearRect graphicsContext
      { x: 0.0
      , y: 0.0
      , width: toNumber canvasWidth
      , height: toNumber canvasHeight
      }

  alterFingering :: Int -> Fingering -> Fingering
  alterFingering fingeredKey fingering =
    if contains fingering fingeredKey then
      filter (\x -> x /= fingeredKey) fingering
    else
      cons fingeredKey fingering
