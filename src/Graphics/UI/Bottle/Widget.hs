{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell, DeriveFunctor, TupleSections, EmptyDataDecls, GADTs #-}
module Graphics.UI.Bottle.Widget (
  Widget(..), Position(..), Focusable(..),
  image, eventMap, takesFocus,
  atImageWithSize, atImage, liftView,
  getFocusable,
  strongerKeys, weakerKeys,
  drawNoFocus, maybeFocusable) where

-- TODO:
-- Cursor type is related to the widget type.

-- The cursor needs to persist, but the widget is recreated from
-- scratch

-- How do we guarantee, statically, that the generated widget will
-- match the persisted cursor?

import Data.Record.Label (getL, modL, mkLabels, (:->), lens)
import Data.Monoid (Monoid(..))
import Graphics.DrawingCombinators.Utils(Image)
import Graphics.UI.Bottle.EventMap (EventMap)
import Graphics.UI.Bottle.SizeRange (Size)
import Graphics.UI.Bottle.Sized (Sized)
import qualified Graphics.UI.Bottle.Sized as Sized

result :: (b -> c) -> (a -> b) -> a -> c
result = (.)

data UserIO a = UserIO {
  _image :: Image,
  _eventMap :: EventMap a
  }
  deriving (Functor)
$(mkLabels [''UserIO])

atEventMap :: (EventMap a -> EventMap b) -> UserIO a -> UserIO b
atEventMap f (UserIO i em) = UserIO i (f em)

data Position = Position -- TODO

data Focusable cursor a = Focusable {
  enter :: Position -> cursor,
  mkUserIO :: cursor -> Sized (UserIO (cursor, a))
  }
  deriving (Functor)

atMkUserIO ::
  ((cursor -> Sized (UserIO (cursor, a))) ->
   cursor -> Sized (UserIO (cursor, b))) ->
  Focusable cursor a -> Focusable cursor b
atMkUserIO f (Focusable e mu) = Focusable e (f mu)

-- mcursor
data RejectsFocus
data AcceptsFocus cursor

data MaybeFocusable mcursor a where
  NoFocusable :: MaybeFocusable RejectsFocus a
  HaveFocusable :: Focusable cursor a -> MaybeFocusable (AcceptsFocus cursor) a

instance Functor (MaybeFocusable mcursor) where
  fmap _ NoFocusable = NoFocusable
  fmap f (HaveFocusable focusable) = HaveFocusable $ fmap f focusable

data Widget mcursor a = Widget {
  _maybeFocusable :: MaybeFocusable mcursor a,
  _drawNoFocus :: Sized Image
  } deriving (Functor)
$(mkLabels [''Widget])

-- modL is too monomorphic
atMaybeFocusable ::
  (MaybeFocusable mcursor0 a0 -> MaybeFocusable mcursor1 a1) ->
  Widget mcursor0 a0 -> Widget mcursor1 a1
atMaybeFocusable f (Widget mf dnf) = Widget (f mf) dnf

atHaveFocusable ::
  (Focusable cursor a -> Focusable cursor b) ->
  Widget (AcceptsFocus cursor) a -> Widget (AcceptsFocus cursor) b
atHaveFocusable chgFocusable = atMaybeFocusable chgMaybeFocusable
  where
    chgMaybeFocusable (HaveFocusable focusable) = HaveFocusable (chgFocusable focusable)

liftView :: Sized Image -> Widget RejectsFocus a
liftView = Widget NoFocusable

atEventMapWithCursor ::
  (cursor -> EventMap (cursor, a) -> EventMap (cursor, b)) ->
  Widget (AcceptsFocus cursor) a -> Widget (AcceptsFocus cursor) b
atEventMapWithCursor mkEventMap =
  atHaveFocusable . atMkUserIO $ inMkUserIO
  where
    inMkUserIO cursorToSized cursor =
      (fmap . atEventMap) (mkEventMap cursor)
        (cursorToSized cursor)

strongerKeys :: EventMap a -> Widget (AcceptsFocus cursor) a -> Widget (AcceptsFocus cursor) a
strongerKeys em = atEventMapWithCursor f
  where
    f cursor oldEventMap = mappend oldEventMap $ fmap (cursor,) em

weakerKeys :: EventMap a -> Widget (AcceptsFocus cursor) a -> Widget (AcceptsFocus cursor) a
weakerKeys em = atEventMapWithCursor f
  where
    f cursor oldEventMap =
      flip mappend oldEventMap $ fmap (cursor,) em

atImageWithSize ::
  (Size -> Image -> Image) -> Widget mcursor a -> Widget mcursor a
atImageWithSize chgImage =
  (modL drawNoFocus . Sized.atFromSize) mkImageWrap .
  atMaybeFocusable chgFocusable
  where
    chgFocusable NoFocusable = NoFocusable
    chgFocusable (HaveFocusable focusable) =
      HaveFocusable $ (atMkUserIO . result . Sized.atFromSize)
                      mkUserIOWrap focusable

    -- mkUserIOWrap :: (Size -> UserIO (cursor, a)) ->
    --                  Size -> UserIO (cursor, a)
    mkUserIOWrap makeUserIO size = modL image (chgImage size) (makeUserIO size)

    mkImageWrap mkImage size = chgImage size (mkImage size)

atImage :: (Image -> Image) -> Widget mcursor a -> Widget mcursor a
atImage = atImageWithSize . const

getFocusable :: Widget (AcceptsFocus cursor) a -> Focusable cursor a
getFocusable Widget { _maybeFocusable = HaveFocusable focusable } = focusable

takesFocus :: Widget RejectsFocus a -> Widget (AcceptsFocus ()) a
takesFocus widget =
  (atMaybeFocusable . const) (HaveFocusable focusable) widget
  where
    makeUserIO img = UserIO { _image = img, _eventMap = mempty }
    focusable = Focusable {
      enter = const (),
      mkUserIO = const . fmap makeUserIO $ getL drawNoFocus widget
      }

-- whenFocused :: (Widget k -> Widget k) -> Widget k -> Widget k
-- whenFocused f widget = Widget mkSized
--   where
--     mkSized hf = (unpack . if hf then f else id) widget hf
