-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import Miso hiding (URI(..))
import Miso.Html
import Miso.Html.Property
import Miso.Lens
import Miso.Router
import qualified Miso.CSS as Style
import GHC.Generics
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
type Model = URI
-----------------------------------------------------------------------------
-- | Action
data Action
  = SetURI URI
  | PushURI Route
  deriving (Show, Eq)
-----------------------------------------------------------------------------
data Route = Index | Home | About | The404
  deriving anyclass Router
  deriving stock (Show, Eq, Generic)
-----------------------------------------------------------------------------
-- | Main entry point
main :: IO ()
main = run $
  miso $ \url ->
    (component url updateModel viewModel)
       { subs = [ uriSub SetURI ]
       }
-----------------------------------------------------------------------------
-- | Update your model
updateModel :: Action -> Transition Model Action
updateModel = \case
  SetURI u -> do
    this .= u
  PushURI route ->
    io_ (pushURI (toURI route))
-----------------------------------------------------------------------------
-- | View function, with routing
viewModel :: Model -> View Model Action
viewModel uri =
  case route uri of
    Left _ -> the404
    Right v -> 
      div_ 
      [] 
      [ h1_
        [ Style.style_ ["font-family" =: "monospace"] ]
        [ "🍜 🌐 miso-router" ]
      , case v of
          Home -> home
          About -> about
          The404 -> the404
          Index -> home
      ]
  where
    home =
        div_
        []
        [ div_ [] [text "home"]
        , button_ [onClick goAbout] [text "go about"]
        ]
    about =
        div_
        []
        [ div_ [] [text "about"]
        , button_ [onClick goHome] [text "go home"]
        ]
    the404 =
        div_
        []
        [ text "the 404 :("
        , button_ [onClick goHome] [text "go home"]
        ]
-----------------------------------------------------------------------------
-- | Type-level routes
-----------------------------------------------------------------------------
goHome, goAbout :: Action
goHome = PushURI Home
goAbout = PushURI About
-----------------------------------------------------------------------------
