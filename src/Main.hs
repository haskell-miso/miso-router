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
import Miso
import Miso.Html
import Miso.Html.Property
import Miso.Lens
import Miso.Router hiding (href_)
import qualified Miso.CSS as Style
import GHC.Generics
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
-- | Action
data Action
  = SetRoute Route
  | PushRoute Route
  | RouterError MisoString
  deriving (Show, Eq)
-----------------------------------------------------------------------------
data Route = Index | Home | About | The404
  deriving anyclass Router
  deriving stock (Show, Eq, Generic)
-----------------------------------------------------------------------------
-- | Main entry point
main :: IO ()
main = do
  startApp defaultEvents
    (component Index updateModel viewModel)
       { subs = [ routerSub (either (RouterError . ms . show) SetRoute) ]
       , logLevel = DebugAll
       }
-----------------------------------------------------------------------------
-- | Update your model
updateModel :: Action -> Effect parent Route Action
updateModel = \case
  SetRoute newRoute ->
    this .= newRoute
  RouterError err -> do
    io_ (consoleError err)
    this .= The404
  PushRoute route ->
    io_ (pushRoute route)
-----------------------------------------------------------------------------
-- | View function, with routing
viewModel :: Route -> View Route Action
viewModel v =
  div_ 
  [] 
  [ h1_
    [ Style.style_ ["font-family" =: "monospace"] ]
    [ "🍜 🌐 ", a_ [ href_ "https://github.com/haskell-miso/miso-router" ] [ "miso-router" ] ]
  , h2_
    [ Style.style_ ["font-family" =: "monospace"] 
    ]
    [ case v of
        Home -> home
        About -> about
        The404 -> the404
        Index -> home
    ]
  ] where
      home =
          div_
          []
          [ div_ [ ] [text "home"]
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
goHome = PushRoute Home
goAbout = PushRoute About
-----------------------------------------------------------------------------
