{-# OPTIONS -Wall #-}
import Prelude hiding (lookup)

import qualified GLFWWrap

import Control.Arrow (first, second)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad
import Control.Newtype (unpack)
import Data.IORef
import Data.List.Utils (enumerate2d)
import Data.Maybe
import Data.Set(Set)
import Data.Time.Clock
import Data.Vector.Vector2(Vector2(..))
import EventMap
import Graphics.DrawingCombinators((%%))
import Graphics.UI.GLFW
import SizeRange (Size)
import Sized (fromSize)
import Widget(Widget(..))
import qualified Data.Set as Set
import qualified FocusDelegator
import qualified Graphics.DrawingCombinators as Draw
import qualified GridEdit
import qualified System.Info
import qualified TextEdit
import qualified Widget

data TypematicState = NoKey | TypematicRepeat { _tsKey :: Key, _tsCount :: Int, _tsStartTime :: UTCTime }

type Model = ([[(FocusDelegator.Model, TextEdit.Model)]],
              GridEdit.Model)

typematicKeyHandlerWrap :: (Int -> NominalDiffTime) -> (Key -> Bool -> IO ()) -> IO (Key -> Bool -> IO ())
typematicKeyHandlerWrap timeFunc handler = do
  stateVar <- newMVar NoKey
  _ <- forkIO . forever $ do
    sleepTime <- modifyMVar stateVar typematicIteration
    threadDelay . round $ 1000000 * sleepTime

  return $ \key isPress -> do
    newValue <-
      if isPress
        then fmap (TypematicRepeat key 0) getCurrentTime
        else return NoKey

    _ <- swapMVar stateVar newValue
    handler key isPress

  where
    typematicIteration state@(TypematicRepeat key count startTime) = do
      now <- getCurrentTime
      let timeDiff = diffUTCTime now startTime
      if timeDiff >= timeFunc count
        then do
          handler key True
          return (TypematicRepeat key (count + 1) startTime,
                  timeFunc (count + 1) - timeDiff)
        else
          return (state, timeFunc count - timeDiff)
    typematicIteration state@NoKey = return (state, timeFunc 0)

modStateFromKeySet :: Set Key -> ModState
modStateFromKeySet keySet =
  ModState {
    modCtrl = isPressed [KeyLeftCtrl, KeyRightCtrl],
    modMeta = False, -- TODO: GLFW doesn't support meta/winkey?
    modAlt = isPressed [KeyLeftAlt, KeyRightAlt],
    modShift = isPressed [KeyLeftShift, KeyRightShift]
    }
  where
    isPressed = any (`Set.member` keySet)

modifiersEventHandlerWrap :: (Event -> IO ()) -> IO (GLFWWrap.GLFWEvent -> IO ())
modifiersEventHandlerWrap wrappedHandler = do
  keySetVar <- newIORef Set.empty
  let
    handler (GLFWWrap.KeyEvent key True) = do
      modifyIORef keySetVar (Set.insert key)
      keySet <- readIORef keySetVar
      wrappedHandler $ KeyEvent (modStateFromKeySet keySet) key
    handler (GLFWWrap.KeyEvent key False) =
      modifyIORef keySetVar (Set.delete key)
    handler (GLFWWrap.CharEvent char True) = do
      keySet <- readIORef keySetVar
      when (modStateFromKeySet keySet `elem` [noMods, shift]) . wrappedHandler $ CharEvent char
    handler _ = return ()
  return handler

defaultFont :: String -> FilePath
defaultFont "darwin" = "/Library/Fonts/Arial.ttf"
defaultFont _ = "/usr/share/fonts/truetype/freefont/FreeSerifBold.ttf"

main :: IO ()
main = GLFWWrap.withGLFW $ do
  font <- Draw.openFont (defaultFont System.Info.os)
  GLFWWrap.openWindow defaultDisplayOptions

  modelVar <- newIORef (replicate 3 . replicate 3 $ (False, TextEdit.Model 4 "Text"),
                        Vector2 0 0)
                        --TextEdit.Model 0 "Text"]

  modifiersHandler <- modifiersEventHandlerWrap (modifyIORef modelVar . updateModel font)

  let
    keyHandler key isPress = modifiersHandler $ GLFWWrap.KeyEvent key isPress
    typematicTime x = 0.5 + fromIntegral x * 0.05

  typematicKeyHandler <- typematicKeyHandlerWrap typematicTime keyHandler

  let
    handleEvent (GLFWWrap.KeyEvent key isPress) = typematicKeyHandler key isPress
    handleEvent GLFWWrap.WindowClose = error "Quit"
    handleEvent x = modifiersHandler x

  GLFWWrap.eventLoop $ \events -> do
    mapM_ handleEvent events
    Draw.clearRender .
      (Draw.scale (20/800) (-20/600) %%) .
      ($ fullSize) . ($ True) . Widget.image . widget font =<<
      readIORef modelVar

fullSize :: Size
fullSize = Vector2 800 600

nth :: Int -> (a -> a) -> [a] -> [a]
nth _ _ [] = error "Apply out of bounds"
nth 0 f (x:xs) = f x : xs
nth n f (x:xs) = x : nth (n-1) f xs

widget :: Draw.Font -> Model -> Widget Model
widget font origModel@(rowModels, gModel) =
  GridEdit.make ((,) rowModels) gModel children
  where
    children = (map . map . uncurry) makeTextEdit . enumerate2d $ rowModels

    makeTextEdit index (fdModel, teModel) =
      FocusDelegator.make (liftRowModel index . first . const) fdModel .
      fmap (liftRowModel index . second . const) .
      TextEdit.make font "<empty>" 2 $
      teModel

    liftRowModel (rowIndex, colIndex) editCell =
      (first . nth rowIndex . nth colIndex) editCell origModel

updateModel :: Draw.Font -> Event -> Model -> Model
updateModel font event model =
  fromMaybe model .
  lookup event .
  fromJust . snd $ fromSize (unpack (widget font model) True) fullSize
