{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses #-}
module Graphics.UI.Bottle.Widget (
  FWidget(..), MEnter, Direction,
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

type MEnter k = Maybe (Direction -> k)

data UserIO k = UserIO {
  uioFrame :: Frame,
  uioMaybeEnter :: MEnter k, -- Nothing if we're not enterable
  uioEventMap :: EventMap k
  }
  deriving (Functor)

atUioFrame :: (Frame -> Frame) -> UserIO a -> UserIO a
atUioMaybeEnter :: (MEnter a -> MEnter a) -> UserIO a -> UserIO a
atUioEventMap :: (EventMap a -> EventMap a) -> UserIO a -> UserIO a
atUioFrame      f u = u { uioFrame      = f . uioFrame      $ u }
atUioMaybeEnter f u = u { uioMaybeEnter = f . uioMaybeEnter $ u }
atUioEventMap   f u = u { uioEventMap   = f . uioEventMap   $ u }

data FWidget a = FWidget {
  wIsFocused :: Bool,
  wContent :: Sized (UserIO a)
  }
  deriving (Functor)

atIsFocused :: (Bool -> Bool) -> FWidget a -> FWidget a
atIsFocused f w = w { wIsFocused = f (wIsFocused w) }

atContent ::
  (Sized (UserIO a) -> Sized (UserIO b)) ->
  FWidget a -> FWidget b
atContent f w = w { wContent = f (wContent w) }

liftView :: Sized Frame -> FWidget a
liftView view =
  FWidget {
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

atUserIO :: (UserIO a -> UserIO b) -> FWidget a -> FWidget b
atUserIO = atContent . fmap

removeExtraSize :: FWidget a -> FWidget a
removeExtraSize = atContent f
  where
    f sized = (Sized.atFromSize . argument) (liftA2 cap maxSize) sized
      where
        cap Nothing y = y
        cap (Just x) y = min x y
        maxSize = getL SizeRange.srMaxSize $ Sized.requestedSize sized

atImageWithSize :: (Size -> Frame -> Frame) -> FWidget a -> FWidget a
atImageWithSize f = atContent . Sized.atFromSize $ g
  where
    g mkUserIO size = atUioFrame (f size) (mkUserIO size)

atImage :: (Frame -> Frame) -> FWidget a -> FWidget a
atImage = atUserIO . atUioFrame

userIO :: FWidget a -> Size -> UserIO a
userIO = Sized.fromSize . wContent

image :: FWidget a -> Size -> Frame
image = (fmap . fmap) uioFrame userIO

eventMap :: FWidget a -> Size -> EventMap a
eventMap = (fmap . fmap) uioEventMap userIO

enter :: FWidget a -> Size -> Maybe (Direction -> a)
enter = (fmap . fmap) uioMaybeEnter userIO

-- ^ If widget already takes focus, it is untouched
takesFocus :: (Direction -> a) -> FWidget a -> FWidget a
takesFocus = atMaybeEnter . const . Just

atMaybeEnter :: (MEnter a -> MEnter a) -> FWidget a -> FWidget a
atMaybeEnter = atUserIO . atUioMaybeEnter

atEventMap :: (EventMap a -> EventMap a) -> FWidget a -> FWidget a
atEventMap = atUserIO . atUioEventMap

-- ^ If doesn't take focus, event map is ignored
strongerKeys :: EventMap a -> FWidget a -> FWidget a
strongerKeys = atEventMap . mappend

-- ^ If doesn't take focus, event map is ignored
weakerKeys :: EventMap a -> FWidget a -> FWidget a
weakerKeys = atEventMap . flip mappend

backgroundColor :: Anim.AnimId -> Draw.Color -> FWidget a -> FWidget a
backgroundColor animId = atImageWithSize . Anim.backgroundColor animId 10
