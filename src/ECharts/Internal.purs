module ECharts.Internal where

import Prelude

import Foreign (Foreign)

foreign import unsafeSetField
  ∷ Foreign → String → Foreign → Foreign

foreign import emptyObject
  ∷ Unit → Foreign

foreign import undefinedValue
  ∷ Foreign
