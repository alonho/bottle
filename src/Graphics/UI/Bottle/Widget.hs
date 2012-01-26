{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
module Graphics.UI.Bottle.Widget (
  Widget(..), MEnter, Direction, Cursor,
  UserIO(..), atUioMaybeEnter, atUioEventMap, atUioFrame,
  EventResult(..), atEAnimIdMapping, atECursor,
  emptyEventResult, eventResultFromCursor,
  actionEventMap, actionEventMapMovesCursor,
  EventHandlers, atContent, atIsFocused,
  userIO, image, eventMap, enter,
  takesFocus, atMkUserIO, atUserIO,
  atImageWithSize, atImage, atMaybeEnter, atEventMap, atEvents,
  backgroundColor, liftView,
  strongerEvents, weakerEvents, align) where

import Control.Applicative ((<$>), (<*>))
import Data.Monoid (Monoid(..))
import Data.Vector.Vector2 (Vector2)
import Graphics.UI.Bottle.Animation (Frame)
import Graphics.UI.Bottle.EventMap (EventMap)
import Graphics.UI.Bottle.SizeRange (Size)
import Graphics.UI.Bottle.Sized (Sized)
import qualified Data.AtFieldTH as AtFieldTH
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as EventMap
import qualified Graphics.UI.Bottle.SizeRange as SizeRange
import qualified Graphics.UI.Bottle.Sized as Sized

type Direction = Maybe (Vector2 Int)

type MEnter f = Maybe (Direction -> f EventResult)

-- TODO: EventResult will also include an AnimIds mapping
type Cursor = Anim.AnimId

data EventResult = EventResult {
  eCursor :: Maybe Cursor,
  eAnimIdMapping :: Anim.AnimId -> Anim.AnimId
  }

AtFieldTH.make ''EventResult

emptyEventResult :: EventResult
emptyEventResult = EventResult {
  eCursor = Nothing,
  eAnimIdMapping = id
  }

eventResultFromCursor :: Cursor -> EventResult
eventResultFromCursor cursor = EventResult {
  eCursor = Just cursor,
  eAnimIdMapping = id
  }

type EventHandlers f = EventMap (f EventResult)

data UserIO f = UserIO {
  uioFrame :: Frame,
  uioMaybeEnter :: MEnter f, -- Nothing if we're not enterable
  uioEventMap :: EventHandlers f
  }

AtFieldTH.make ''UserIO

data Widget f = Widget {
  isFocused :: Bool,
  content :: Sized (UserIO f)
  }

AtFieldTH.make ''Widget

atEvents :: (f EventResult -> g EventResult) -> Widget f -> Widget g
atEvents func =
  atUserIO chg
  where
    chg userIo = userIo {
      uioMaybeEnter = (fmap . fmap) func $ uioMaybeEnter userIo,
      uioEventMap = fmap func $ uioEventMap userIo
      }

liftView :: Sized Frame -> Widget f
liftView view =
  Widget {
    isFocused = False,
    content = fmap buildUserIO view
    }
  where
    buildUserIO frame =
      UserIO {
        uioFrame = frame,
        uioEventMap = mempty,
        uioMaybeEnter = Nothing
        }

atUserIO :: (UserIO f -> UserIO g) -> Widget f -> Widget g
atUserIO = atContent . fmap

atMkUserIO :: ((Size -> UserIO f) -> Size -> UserIO f) -> Widget f -> Widget f
atMkUserIO = atContent . Sized.atFromSize

atImageWithSize :: (Size -> Frame -> Frame) -> Widget f -> Widget f
atImageWithSize f = atMkUserIO g
  where
    g mkUserIO size = atUioFrame (f size) (mkUserIO size)

atImage :: (Frame -> Frame) -> Widget f -> Widget f
atImage = atUserIO . atUioFrame

userIO :: Widget f -> Size -> UserIO f
userIO = Sized.fromSize . content

image :: Widget f -> Size -> Frame
image = (fmap . fmap) uioFrame userIO

eventMap :: Widget f -> Size -> EventHandlers f
eventMap = (fmap . fmap) uioEventMap userIO

enter :: Widget f -> Size -> Maybe (Direction -> f EventResult)
enter = (fmap . fmap) uioMaybeEnter userIO

-- ^ If Widget already takes focus, it is untouched
-- TODO: Would be nicer as (Direction -> Cursor), but then TextEdit's "f" couldn't be ((,) String)..
takesFocus :: Functor f => (Direction -> f Cursor) -> Widget f -> Widget f
takesFocus = atMaybeEnter . const . Just . (fmap . fmap) eventResultFromCursor

atMaybeEnter :: (MEnter f -> MEnter f) -> Widget f -> Widget f
atMaybeEnter = atUserIO . atUioMaybeEnter

atEventMap :: (EventHandlers f -> EventHandlers f) -> Widget f -> Widget f
atEventMap = atUserIO . atUioEventMap

-- ^ If doesn't take focus, event map is ignored
strongerEvents :: EventHandlers f -> Widget f -> Widget f
strongerEvents = atEventMap . mappend

-- ^ If doesn't take focus, event map is ignored
weakerEvents :: EventHandlers f -> Widget f -> Widget f
weakerEvents = atEventMap . flip mappend

backgroundColor :: Anim.AnimId -> Draw.Color -> Widget f -> Widget f
backgroundColor animId = atImageWithSize . Anim.backgroundColor animId 10

makeActionEventMap ::
  [EventMap.EventType] -> EventMap.Doc -> a -> EventMap a
makeActionEventMap keys doc act =
  mconcat $ map (flip (`EventMap.fromEventType` doc) act) keys

actionEventMap ::
  Functor f => [EventMap.EventType] -> EventMap.Doc ->
  f () -> EventHandlers f
actionEventMap keys doc act =
  (fmap . fmap . const) emptyEventResult $
  makeActionEventMap keys doc act

actionEventMapMovesCursor ::
  Functor f => [EventMap.EventType] -> EventMap.Doc ->
  f Cursor -> EventHandlers f
actionEventMapMovesCursor keys doc act =
  (fmap . fmap) eventResultFromCursor $
  makeActionEventMap keys doc act

-- If widget's max size is smaller than given size, place widget in
-- portion of the extra space (0..1 ratio in each dimension):
align :: Vector2 Draw.R -> Widget f -> Widget f
align ratio = atContent f
  where
    f sized = Sized.atFromSize ((g . SizeRange.srMaxSize . Sized.requestedSize) sized) sized
    g maxSize mkSize size =
      atUioFrame (Anim.translate pos) . mkSize $
      cap <$> maxSize <*> size
      where
        pos = mkPos <$> maxSize <*> ratio <*> size
        mkPos Nothing _ _ = 0
        mkPos (Just maxSz) r sz = r * (sz - min maxSz sz)
        cap Nothing sz = sz
        cap (Just maxSz) sz = min maxSz sz
