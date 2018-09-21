module ECharts.Event (listenAll, dispatch, on_) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEff, class MonadEffect)
import Control.Monad.Except (runExcept)
import Data.Foldable (for_)
import Foreign (Foreign, readString)
import Foreign.Index (readProp)
import Data.List as L
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant.Internal (RLProxy(..), variantTags, class VariantTags)
import ECharts.Types (EChartsEvent, EChartsEventR, Chart, ECHARTS)
import Type.Row (class RowToList)
import Unsafe.Coerce (unsafeCoerce)

foreign import on_
  ∷ ∀ e
  . Chart
  → String
  → ( Foreign → Effect (echarts ∷ ECHARTS|e) Unit )
  → Effect (echarts ∷ ECHARTS|e) Unit

listenAll
  ∷ ∀ e m
  . MonadEffect ( echarts ∷ ECHARTS |e ) m
  ⇒ Chart
  → ( EChartsEvent → Effect (echarts ∷ ECHARTS|e) Unit )
  → m Unit
listenAll chart cb = liftEff $
  for_ eventNames \en → on_ chart en \frn →
    for_ (runExcept $ readProp "type" frn >>= readString) \tp →
      when (tp == en) $ cb $ toEChartsEvent $ Tuple tp frn
  where
  eventNames ∷ ∀ rl. RowToList EChartsEventR rl ⇒ VariantTags rl ⇒ L.List String
  eventNames = variantTags (RLProxy ∷ RLProxy rl)

  toEChartsEvent ∷ ∀ ω. Tuple String ω → EChartsEvent
  toEChartsEvent = unsafeCoerce

foreign import dispatchAction_
  ∷ ∀ e action
  . action
  → Chart
  → Effect ( echarts ∷ ECHARTS |e ) Unit

dispatch
  ∷ ∀ e m
  . MonadEffect ( echarts ∷ ECHARTS | e ) m
  ⇒ EChartsEvent
  → Chart
  → m Unit
dispatch vaction chart =
  liftEff $ dispatchAction_ action chart
  where
  variantPair ∷ Tuple String {}
  variantPair = unsafeCoerce vaction

  actionType ∷ String
  actionType = case fst variantPair of
    "legendselectchanged" → "legendToggleSelect"
    "legendselected" → "legendSelect"
    "legendunselected" → "legendUnSelect"
    "datazoom" → "dataZoom"
    "datarangeselected" → "selectDataRange"
    "timelinechanged" → "timelineChange"
    "timelineplaychanged" → "timelinePlayChange"
    "pieselectchanged" → "pieToggleSelect"
    "pieselected" → "pieSelect"
    "pieunselected" → "pieUnSelect"
    "mapselectchanged" → "mapToggleSelect"
    "mapselected" → "mapSelect"
    "mapunselected" → "mapUnSelect"
    "focusnodeadjacency" → "focusNodeAdjacency"
    "unfocusnodeadjacency" → "unfocusNodeAdjacency"
    s → s

  action ∷ ∀ ω. { "type" ∷ String | ω }
  action = unsafeSet "type" actionType $ snd variantPair
