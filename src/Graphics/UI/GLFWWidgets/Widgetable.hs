{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, TypeSynonymInstances,
             TupleSections #-}
module Graphics.UI.GLFWWidgets.Widgetable(Widgetable(..), Theme(..)) where

import qualified Graphics.UI.GLFWWidgets.TextEdit as TextEdit
import qualified Graphics.UI.GLFWWidgets.GridEdit as GridEdit
import qualified Graphics.UI.GLFWWidgets.FocusDelegator as FocusDelegator
import           Graphics.UI.GLFWWidgets.Widget(Widget)
import           Data.List.Utils (enumerate2d, nth)
import           Control.Arrow (second)

data Theme = Theme {
  textEditTheme :: TextEdit.Theme
  }

class Widgetable a where
  toWidget :: Theme -> a -> Widget a

-- TODO: Move into polymorphic/constrained theme
ptSize :: Int
ptSize = 40

instance Widgetable TextEdit.Model where
  toWidget theme = TextEdit.make (textEditTheme theme) ptSize

instance Widgetable a => Widgetable (GridEdit.Cursor, [[a]]) where
  toWidget theme (cursor, childrenModels) =
    GridEdit.make (, childrenModels) cursor .
      (map . map) (
        liftInnerWidget .
        second (toWidget theme)
      ) .
      enumerate2d $
      childrenModels
    where
      setChildModel (rowIndex, colIndex) x =
        (nth rowIndex . nth colIndex . const) x childrenModels
      liftInnerWidget (index, childWidget) =
        fmap ((cursor,) . setChildModel index) childWidget

instance Widgetable a => Widgetable (FocusDelegator.Cursor, a) where
  toWidget theme (cursor, childModel) =
    FocusDelegator.make (, childModel) cursor .
    fmap (cursor,) . toWidget theme $
    childModel
