-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import Data.Proxy
import Servant.API
import Servant.Links
-----------------------------------------------------------------------------
import Miso
import Miso.Lens
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
  | PushURI URI
  deriving (Show, Eq)
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
updateModel (SetURI u)  = this .= u
updateModel (PushURI u) = io_ (pushURI u)
-----------------------------------------------------------------------------
-- | View function, with routing
viewModel :: Model -> View Model Action
viewModel uri =
  case route (Proxy :: Proxy API) (about :<|> home) id uri of
    Left _ -> the404
    Right v -> 
      div_ 
      [] 
      [ h1_
        [ CSS.style ["font-family" =: "monospace"] ]
        [ "🍜 🌐 miso-router" ]
      , v
      ]
  where
    home (_ :: Model) =
        div_
        []
        [ div_ [] [text "home"]
        , button_ [onClick goAbout] [text "go about"]
        ]
    about (_ :: Model) =
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
type API = About :<|> Home
type Home = View Model Action
type About = "about" :> View Model Action
-----------------------------------------------------------------------------
-- | Type-safe links used in `onClick` event handlers to route the application
aboutUri, homeUri :: URI
aboutUri :<|> homeUri = allLinks' linkURI (Proxy @API)
-----------------------------------------------------------------------------
goHome, goAbout :: Action
goHome = PushURI homeUri
goAbout = PushURI aboutUri
-----------------------------------------------------------------------------
