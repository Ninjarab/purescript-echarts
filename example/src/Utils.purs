module Utils
  ( randomInArray
  , getElementById
  , randomArray
  , precise
  , onLoad
  ) where

import Prelude

import Effect (Effect)
import Effect.Random (random)
import Control.Monad.Except (runExcept)

import Data.Array ((!!), length)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust)
import Data.Either (either)
import Data.Tuple (Tuple(..))
import Foreign (toForeign)
import Data.NonEmpty as NE

import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (load)
import DOM.HTML.Types (HTMLElement, windowToEventTarget, htmlDocumentToNonElementParentNode, readHTMLElement)
import DOM.HTML.Window (document)
import Web.DOM.NonElementParentNode as NEPN
import DOM.Node.Types (ElementId)

import Math (round, pow)
import Partial.Unsafe (unsafePartial)

getElementById ∷ ElementId → Effect (Maybe HTMLElement)
getElementById elementId = do
  win ← window
  doc ← document win
  el ← NEPN.getElementById elementId (htmlDocumentToNonElementParentNode doc)
  pure $ either (const Nothing) Just $ runExcept $ readHTMLElement (toForeign <<< unsafePartial fromJust $ el)

onLoad
  ∷ ∀ a
  . Effect a
  → Effect Unit
onLoad handler =
  addEventListener load (eventListener (const handler)) false
    <<< windowToEventTarget
    =<< window

precise ∷ Number → Number → Number
precise pre num =
  (round $ (pow 10.0 pre) * num) / (pow 10.0 pre)

foreign import randomArrayImpl
  ∷ ∀ a
  . (a → Array a → NE.NonEmpty Array a)
  → Int
  → Effect (NE.NonEmpty Array Number)

randomArray ∷ Int → Effect (NE.NonEmpty Array Number)
randomArray = randomArrayImpl NE.NonEmpty

randomInArray
  ∷ ∀ a
  . NE.NonEmpty Array a
  → Effect (Tuple a Int)
randomInArray nelst = do
  rnd ← random
  let
    lst = NE.oneOf nelst
    l = length lst
    i = Int.floor (rnd * Int.toNumber l)
  pure $ case lst !! i of
    Nothing → Tuple (NE.head nelst) 0
    Just x → Tuple x i
