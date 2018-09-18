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
import Effect.Class (class MonadEffect, liftEff)
import Data.Either (either)
import Foreign (Foreign, toForeign)
import DOM.HTML.Types (HTMLElement)
import ECharts.Internal (undefinedValue)
import ECharts.Theme (Theme, builtInThemeName)
import ECharts.Types (Chart, ECHARTS, Option)

foreign import initImpl
  ∷ Foreign
  → HTMLElement
  → Effect Chart

init
  ∷ ∀ m
  . MonadEffect m
  ⇒ HTMLElement
  → m Chart
init el = liftEff $ initImpl undefinedValue el

initWithTheme
  ∷ ∀ m
  . MonadEffect m
  ⇒ Theme
  → HTMLElement
  → m Chart
initWithTheme theme el =
  liftEff $ initImpl (either (toForeign <<< builtInThemeName) toForeign theme) el

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
setOption opts chart = liftEff $ setOptionImpl opts chart

foreign import resetOptionImpl ∷ Option → Chart → Effect Unit

resetOption
  ∷ ∀ m
  . MonadEffect m
  ⇒ Option
  → Chart
  → m Unit
resetOption opts chart = liftEff $ resetOptionImpl opts chart

foreign import resizeImpl ∷ Chart → Effect Unit

resize
  ∷ ∀ m
  . MonadEffect m
  ⇒ Chart
  → m Unit
resize chart = liftEff $ resizeImpl chart


foreign import clearImpl ∷ Chart → Effect Unit

clear
  ∷ ∀ m
  . MonadEffect m
  ⇒ Chart
  → m Unit
clear chart = liftEff $ clearImpl chart

foreign import disposeImpl ∷ Chart → Effect Unit

dispose
  ∷ ∀ m
  . MonadEffect m
  ⇒ Chart
  → m Unit
dispose chart = liftEff $ disposeImpl chart

foreign import getOptionImpl ∷ Chart → Effect Foreign

getOption
  ∷ ∀ m
  . MonadEffect m
  ⇒ Chart
  → m Foreign
getOption chart = liftEff $ getOptionImpl chart
