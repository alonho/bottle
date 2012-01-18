{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses #-}
module Graphics.UI.Bottle.Widget (
  Widget(..), Direction,
  UserIO(..), atContent, atIsFocused,
  atUioMaybeEnter, atUioEventMap, atUioFrame,
  userIO, image, eventMap, enter,
  takesFocus, atUserIO,
  atImageWithSize, atImage, atMaybeEnter, atEventMap,
  backgroundColor, liftView, removeExtraSize,
  strongerKeys, weakerKeys) where

import Control.Applicative (liftA2)
import Data.Monoid (Monoid(..))
import Data.Record.Label (getL)
import Data.Vector.Vector2 (Vector2)
import Graphics.UI.Bottle.Animation (Frame)
import Graphics.UI.Bottle.EventMap (EventMap)
import Graphics.UI.Bottle.SizeRange (Size)
import Graphics.UI.Bottle.Sized (Sized)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.SizeRange as SizeRange
import qualified Graphics.UI.Bottle.Sized as Sized

type Direction = Maybe (Vector2 Int)

data UserIO k = UserIO {
  uioFrame :: Frame,
  uioMaybeEnter :: Maybe (Direction -> k), -- Nothing if we're not enterable
  uioEventMap :: EventMap k
  }
  deriving (Functor)

atUioFrame ::
  (Frame -> Frame) -> UserIO a -> UserIO a
atUioMaybeEnter ::
  (Maybe (Direction -> a) ->
   Maybe (Direction -> a)) -> UserIO a -> UserIO a
atUioEventMap ::
  (EventMap a -> EventMap a) -> UserIO a -> UserIO a
atUioFrame      f u = u { uioFrame      = f . uioFrame      $ u }
atUioMaybeEnter f u = u { uioMaybeEnter = f . uioMaybeEnter $ u }
atUioEventMap   f u = u { uioEventMap   = f . uioEventMap   $ u }

data Widget a = Widget {
  wIsFocused :: Bool,
  wContent :: Sized (UserIO a)
  }
  deriving (Functor)

atIsFocused :: (Bool -> Bool) -> Widget a -> Widget a
atIsFocused f w = w { wIsFocused = f (wIsFocused w) }

atContent ::
  (Sized (UserIO a) -> Sized (UserIO b)) ->
  Widget a -> Widget b
atContent f w = w { wContent = f (wContent w) }

liftView :: Sized Frame -> Widget a
liftView view =
  Widget {
    wIsFocused = False,
    wContent = fmap buildUserIO view
    }
  where
    buildUserIO frame =
      UserIO {
        uioFrame = frame,
        uioEventMap = mempty,
        uioMaybeEnter = Nothing
        }

argument :: (a -> b) -> (b -> c) -> a -> c
argument = flip (.)

atUserIO :: (UserIO a -> UserIO b) -> Widget a -> Widget b
atUserIO = atContent . fmap

removeExtraSize :: Widget a -> Widget a
removeExtraSize = atContent f
  where
    f sized = (Sized.atFromSize . argument) (liftA2 cap maxSize) sized
      where
        cap Nothing y = y
        cap (Just x) y = min x y
        maxSize = getL SizeRange.srMaxSize $ Sized.requestedSize sized

atImageWithSize :: (Size -> Frame -> Frame) -> Widget a -> Widget a
atImageWithSize f = atContent . Sized.atFromSize $ g
  where
    g mkUserIO size = atUioFrame (f size) (mkUserIO size)

atImage :: (Frame -> Frame) -> Widget a -> Widget a
atImage = atUserIO . atUioFrame

userIO :: Widget a -> Size -> UserIO a
userIO = Sized.fromSize . wContent

image :: Widget a -> Size -> Frame
image = (fmap . fmap) uioFrame userIO

eventMap :: Widget a -> Size -> EventMap a
eventMap = (fmap . fmap) uioEventMap userIO

enter :: Widget a -> Size -> Maybe (Direction -> a)
enter = (fmap . fmap) uioMaybeEnter userIO

-- ^ If widget already takes focus, it is untouched
takesFocus :: (Direction -> a) -> Widget a -> Widget a
takesFocus = atMaybeEnter . const . Just

atMaybeEnter ::
  (Maybe (Direction -> a) ->
   Maybe (Direction -> a)) ->
  Widget a -> Widget a
atMaybeEnter = atUserIO . atUioMaybeEnter

atEventMap :: (EventMap a -> EventMap a) -> Widget a -> Widget a
atEventMap = atUserIO . atUioEventMap

-- ^ If doesn't take focus, event map is ignored
strongerKeys :: EventMap a -> Widget a -> Widget a
strongerKeys = atEventMap . mappend

-- ^ If doesn't take focus, event map is ignored
weakerKeys :: EventMap a -> Widget a -> Widget a
weakerKeys = atEventMap . flip mappend

backgroundColor :: Anim.AnimId -> Draw.Color -> Widget a -> Widget a
backgroundColor animId = atImageWithSize . Anim.backgroundColor animId 10
