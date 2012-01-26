{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.UI.Bottle.Widgets.EventMapDoc(make, addHelp, makeToggledHelpAdder) where

import Data.IORef (newIORef, readIORef, modifyIORef)
import Data.List(sortBy)
import Data.Monoid(mappend)
import Data.Ord(comparing)
import Graphics.UI.Bottle.EventMap(EventMap)
import Graphics.UI.Bottle.SizeRange (srMinSize)
import Graphics.UI.Bottle.Sized (Sized(..))
import Graphics.UI.Bottle.Widget (Widget)
import qualified Data.ByteString.Char8 as SBS8
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Sized as Sized
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.GridView as GridView
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . comparing

make :: EventMap a -> TextView.Style -> Anim.AnimId -> Sized Anim.Frame
make eventMap style animId =
  GridView.make . map toRow . sortOn snd . E.eventMapDocs $ eventMap
  where
    textView str uniq =
      TextView.make style str $
      Anim.joinId animId (map SBS8.pack uniq)
    toRow (eventName, eventDoc) =
      [textView eventName [eventName, "name"],
       textView eventDoc [eventName, "doc"]]

addHelp :: TextView.Style -> Widget f -> Widget f
addHelp style =
  Widget.atMkUserIO f
  where
    f mkUserIO size = Widget.atUioFrame (mappend docFrame) userIO
      where
        rSize = srMinSize (requestedSize eventMapDoc)
        eventMapDoc = make eventMap style ["help box"]
        transparency = Draw.Color 1 1 1
        docFrame =
          (Anim.onImages . Draw.tint . transparency) 0.8 .
          Anim.onDepth (subtract 10) .
          Anim.translate (size - rSize) .
          Anim.backgroundColor ["help doc background"] 1 (Draw.Color 0.3 0.2 0.1 1) rSize $
          Sized.fromSize eventMapDoc size
        eventMap = Widget.uioEventMap userIO
        userIO = mkUserIO size

makeToggledHelpAdder :: [E.EventType] -> TextView.Style -> IO (Widget IO -> IO (Widget IO))
makeToggledHelpAdder overlayDocKeys style = do
  showingHelpVar <- newIORef True
  let
    toggle = modifyIORef showingHelpVar not
    addToggleEventMap doc =
      Widget.strongerEvents $
      Widget.actionEventMap overlayDocKeys doc toggle
  return $ \widget -> do
    showingHelp <- readIORef showingHelpVar
    return $
      if showingHelp
      then addHelp style $ addToggleEventMap "Hide Key Bindings" widget
      else addToggleEventMap "Show Key Bindings" widget
