{-# OPTIONS -Wall #-}
{-# LANGUAGE Rank2Types #-}
module Graphics.UI.Bottle.Widgets.Box(Cursor, make, makeBiased, Orientation, horizontal, vertical) where

import Data.Vector.Vector2(Vector2(..))
import Graphics.UI.Bottle.Widget(Widget)
import qualified Graphics.UI.Bottle.Widgets.Grid as Grid

type Cursor = Int

data Orientation = Orientation {
  oToGridCursor :: Cursor -> Grid.Cursor,
  oToGridChildren :: forall a. [a] -> [[a]]
  }

horizontal :: Orientation
horizontal = Orientation {
  oToGridCursor = (`Vector2` 0),
  oToGridChildren = (: [])
  }

vertical :: Orientation
vertical = Orientation {
  oToGridCursor = (0 `Vector2`),
  oToGridChildren = map (: [])
  }

make :: Orientation -> [Widget k] -> Widget k
make (Orientation _ toGridChildren) = Grid.make . toGridChildren

makeBiased :: Orientation -> Cursor -> [Widget k] -> Widget k
makeBiased (Orientation toGridCursor toGridChildren) cursor =
  Grid.makeBiased (toGridCursor cursor) . toGridChildren
