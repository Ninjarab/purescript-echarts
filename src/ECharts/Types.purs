module ECharts.Types where

import Prelude

import Foreign (Foreign, unsafeToForeign)
import Data.Variant as V
import Data.Set (Set)

foreign import data Chart ∷ Type

data TooltipTrigger
  = ItemTrigger
  | AxisTrigger

tooltipTriggerToForeign ∷ TooltipTrigger → Foreign
tooltipTriggerToForeign = unsafeToForeign <<< case _ of
  ItemTrigger → "item"
  AxisTrigger → "axis"

data PixelOrPercent
  = Pixel Int
  | Percent Number

pixelOrPercentToForeign ∷ PixelOrPercent → Foreign
pixelOrPercentToForeign = case _ of
  Pixel i → unsafeToForeign i
  Percent n → unsafeToForeign $ show n <> "%"

data Orient
  = Vertical
  | Horizontal

orientToForeign ∷ Orient → Foreign
orientToForeign = unsafeToForeign <<< case _ of
  Vertical → "vertical"
  Horizontal → "horizontal"

data AxisType
  = Category
  | Value
  | Time
  | Log

axisTypeToForeign ∷ AxisType → Foreign
axisTypeToForeign = unsafeToForeign <<< case _ of
  Category → "category"
  Value → "value"
  Time → "time"
  Log → "log"

data Symbol
  = Circle
  | Rect
  | RoundRect
  | Triangle
  | Diamond
  | Pin
  | Arrow
  | EmptyCircle
  | None

symbolToForeign ∷ Symbol → Foreign
symbolToForeign = unsafeToForeign <<< case _ of
  Circle → "circle"
  Rect → "rect"
  RoundRect → "roundRect"
  Triangle → "triangle"
  Diamond → "diamond"
  Pin → "pin"
  Arrow → "arrow"
  EmptyCircle → "emptyCircle"
  None → "none"


newtype Point = Point { x ∷ PixelOrPercent, y ∷ PixelOrPercent }
pointToForeign ∷ Point → Foreign
pointToForeign (Point {x, y}) =
  unsafeToForeign [ pixelOrPercentToForeign x, pixelOrPercentToForeign y ]

newtype Radius = Radius { start ∷ PixelOrPercent, end ∷ PixelOrPercent }
radiusToForeign ∷ Radius → Foreign
radiusToForeign (Radius {start, end}) =
  unsafeToForeign [ pixelOrPercentToForeign start, pixelOrPercentToForeign end ]

newtype SingleValueRadius = SingleValueRadius PixelOrPercent
singleValueRadiusToForeign ∷ SingleValueRadius → Foreign
singleValueRadiusToForeign (SingleValueRadius r) = pixelOrPercentToForeign r

numItem ∷ Number → Item
numItem = Item <<< unsafeToForeign

strItem ∷ String → Item
strItem = Item <<< unsafeToForeign

numArrItem ∷ Array Number → Item
numArrItem = Item <<< unsafeToForeign

strArrItem ∷ Array String → Item
strArrItem = Item <<< unsafeToForeign

data PointerType
  = LinePointer
  | CrossPointer
  | ShadowPointer

pointerTypeToForeign ∷ PointerType → Foreign
pointerTypeToForeign = unsafeToForeign <<< case _ of
  LinePointer → "line"
  CrossPointer → "cross"
  ShadowPointer → "shadow"

data LineType
  = SolidLine
  | DashedLine
  | DottedLine

lineTypeToForeign ∷ LineType → Foreign
lineTypeToForeign = unsafeToForeign <<< case _ of
  SolidLine → "solid"
  DashedLine → "dashed"
  DottedLine → "dotted"

pairItem ∷ Number → Number → Item
pairItem x y = Item $ unsafeToForeign [ x, y ]

type FormatterInput =
  { componentType ∷ String
  , seriesIndex ∷ Int
  , seriesName ∷ String
  , name ∷ String
  , dataIndex ∷ Int
  , "data" ∷ Item -- ???
  , value ∷ Foreign
  , color ∷ String
  , percent ∷ Number
  , dataType ∷ String
  }

type FormatterInputArrayValue =
  { componentType ∷ String
  , seriesIndex ∷ Int
  , seriesName ∷ String
  , name ∷ String
  , dataIndex ∷ Int
  , "data" ∷ Item
  , value ∷ Array Foreign
  , color ∷ String
  , percent ∷ Number
  }

data SelectedMode
  = Single
  | Multiple
  | Disabled

selectedModeToForeign ∷ SelectedMode → Foreign
selectedModeToForeign = case _ of
  Single → unsafeToForeign "single"
  Multiple → unsafeToForeign "multiple"
  Disabled → unsafeToForeign false

data HorizontalPosition
  = LeftHP
  | RightHP
  | CenterHP

horizontalPositionToForeign ∷ HorizontalPosition → Foreign
horizontalPositionToForeign = unsafeToForeign <<< case _ of
  LeftHP → "left"
  RightHP → "right"
  CenterHP → "center"

newtype Item = Item Foreign
newtype Coord = Coord Foreign

coord ∷ Number → Number → Coord
coord x y = Coord $ unsafeToForeign [ x, y ]

type LegendEventR =
  { name ∷ String
  , selected ∷ Set Boolean
  }

type DataRangeEventR =
  { visualMapId ∷ String
  , selected ∷ Array Number
  }

type ClickEventR =
  { seriesName ∷ String
  , name ∷ String
  , dataIndex ∷ Int
  , seriesIndex ∷ Int
  }

type BrushEventAreaR =
  { brushType ∷ String
  , panelId ∷ String
  , coordRange ∷ Array Foreign
  , xAxisData ∷ Array String
  , yAxisData ∷ Array String
  , gridIndex ∷ Int
  }

type BrushEventR =
  { areas ∷ Array BrushEventAreaR
  , titleTexts ∷ Array String
  }

type EChartsEventR =
  ( click ∷ ClickEventR
  , dblclick ∷ Foreign
  , mousedown ∷ Foreign
  , mousemove ∷ Foreign
  , mouseup ∷ Foreign
  , mouseover ∷ Foreign
  , mouseout ∷ Foreign
  , legendselectchanged ∷ LegendEventR
  , legendselected ∷ LegendEventR
  , legendunselected ∷ LegendEventR
  , datazoom ∷ Foreign
  , datarangeselected ∷ DataRangeEventR
  , timelinechanged ∷ Foreign
  , timelineplaychanged ∷ Foreign
  , restore ∷ Foreign
  , dataviewchanged ∷ Foreign
  , magictypechanged ∷ Foreign
  -- This is not absolutely accurate, but working with pieselectchanged
  -- is too painful
  , pieselectchanged ∷ ClickEventR
  , pieselected ∷ ClickEventR
  , pieunselected ∷ ClickEventR
  , mapselectchanged ∷ Foreign
  , mapselected ∷ Foreign
  , mapunselected ∷ Foreign
  , axisareaselected ∷ Foreign
  , focusNodeAdjancency ∷ Foreign
  , unfocusNodeAdjacency ∷ Foreign
  , brush ∷ BrushEventR
  , brushselected ∷ BrushEventR
  , pieSelect ∷ Foreign
  , pieUnSelect ∷ Foreign
  )

type EChartsEvent = V.Variant EChartsEventR
newtype Option = Option Foreign
