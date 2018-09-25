module ECharts.Chart
  ( init
  , initWithTheme
  , registerTheme
  , setOption
  , resetOption
  , resize
  , dispose
  , clear
  , getOption
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Data.Either (either)
import Foreign (Foreign, unsafeToForeign)
import Web.DOM.Internal.Types (Element)
import ECharts.Internal (undefinedValue)
import ECharts.Theme (Theme, builtInThemeName)
import ECharts.Types (Chart, Option)

foreign import initImpl
  ∷ Foreign
  → Element
  → Effect Chart

init
  ∷ ∀ m
  . MonadEffect m
  ⇒ Element
  → m Chart
init el = liftEffect $ initImpl undefinedValue el

initWithTheme
  ∷ ∀ m
  . MonadEffect m
  ⇒ Theme
  → Element
  → m Chart
initWithTheme theme el =
  liftEffect $ initImpl (either (unsafeToForeign <<< builtInThemeName) unsafeToForeign theme) el

foreign import registerTheme
  ∷ String
  → Foreign
  → Effect Unit

foreign import setOptionImpl
  ∷ Option → Chart → Effect Unit

setOption
  ∷ ∀ m
  . MonadEffect m
  ⇒ Option
  → Chart
  → m Unit
setOption opts chart = liftEffect $ setOptionImpl opts chart

foreign import resetOptionImpl ∷ Option → Chart → Effect Unit

resetOption
  ∷ ∀ m
  . MonadEffect m
  ⇒ Option
  → Chart
  → m Unit
resetOption opts chart = liftEffect $ resetOptionImpl opts chart

foreign import resizeImpl ∷ Chart → Effect Unit

resize
  ∷ ∀ m
  . MonadEffect m
  ⇒ Chart
  → m Unit
resize chart = liftEffect $ resizeImpl chart


foreign import clearImpl ∷ Chart → Effect Unit

clear
  ∷ ∀ m
  . MonadEffect m
  ⇒ Chart
  → m Unit
clear chart = liftEffect $ clearImpl chart

foreign import disposeImpl ∷ Chart → Effect Unit

dispose
  ∷ ∀ m
  . MonadEffect m
  ⇒ Chart
  → m Unit
dispose chart = liftEffect $ disposeImpl chart

foreign import getOptionImpl ∷ Chart → Effect Foreign

getOption
  ∷ ∀ m
  . MonadEffect m
  ⇒ Chart
  → m Foreign
getOption chart = liftEffect $ getOptionImpl chart
