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
import Data.Array ((!!), length)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.NonEmpty as NE
import Web.Event.EventTarget (eventListener, addEventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (load)
import Web.DOM.Internal.Types (Element)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document, toEventTarget)
import Web.DOM.NonElementParentNode as NEPN
import Math (round, pow)
import Effect.Unsafe (unsafePerformEffect)

getElementById ∷ String → Effect (Maybe Element)
getElementById elementId = do
  win ← window
  doc ← document win
  el ← NEPN.getElementById elementId (toNonElementParentNode doc)
  pure el

onLoad ∷ ∀ a. Effect a → Effect Unit
onLoad handler =
  addEventListener load (unsafePerformEffect $ eventListener (const handler)) false
    <<< toEventTarget
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
