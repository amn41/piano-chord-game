module Navigation.Router where

import Prelude
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (hush)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Navigation.Route (Route(..), routeCodec)
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Navigation.Navigate (class Navigate, navigate)
import Piano.Page as Piano
import Type.Proxy (Proxy(..))

type OpaqueSlot = H.Slot (Const Void) Void

type State = { route :: Maybe Route }

data Query a = Navigate Route a

data Action = Initialize

type ChildSlots = ( piano :: Piano.Slot Unit )

component :: ∀ m. MonadAff m => Navigate m => H.Component Query Unit Void m
component =
  H.mkComponent
    { initialState: \_ -> { route: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleQuery = handleQuery
        , handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      initialRoute <- hush <<< (RD.parse routeCodec) <$> H.liftEffect getHash
      navigate $ fromMaybe Piano initialRoute

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route } <- H.get
      when (route /= Just dest) do
        H.modify_ _ { route = Just dest }
      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route } = case route of
    Just r -> case r of
      Piano -> HH.slot (Proxy :: Proxy "piano") unit Piano.component unit absurd
    Nothing -> HH.slot (Proxy :: Proxy "piano") unit Piano.component unit absurd
