module Evv where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener)
import DOM.Event.MouseEvent (pageX)
import DOM.Event.Types (EventType(..), MouseEvent)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import FRP (FRP)
import FRP.Behavior (Behavior)
import FRP.Behavior.Mouse as Mouse
import FRP.Event.Mouse (down)
import Graphics.Canvas (getCanvasElementById, getContext2D)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit)
import Graphics.Canvas (CANVAS, CanvasElement, Context2D, translate, arc, clearRect, closePath, fillPath, fillText, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, lineTo, moveTo, setFillStyle, setStrokeStyle, strokePath)



type Postion =  {x :: Number, y :: Number}

--test :: forall a. Behavior({x :: Number, y :: Number}) -> Eff(console :: CONSOLE | a) Unit
--test {x : xx, y : yy} = void $ unsafePartial do
--  log (show xx)
--  log (show yy)

test :: forall a. MouseEvent -> Eff(console :: CONSOLE, dom :: DOM | a) Unit
test aa = do
  log (show (pageX aa))
  pure unit



main :: forall e.Eff(canvas :: CANVAS, console :: CONSOLE, dom :: DOM| e) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  pure unit
--  addEventListener (EventType "click") test false

--  test Mouse.position-- <#> maybe { x: 0.0, y: 0.0 } (\{ x, y } -> { x: toNumber x, y: toNumber y })






{-
import Control.Monad.Eff
import Control.Monad.Eff.Timer
import DOM
import Prelude
import Signal
import Signal.DOM

type Dimensions = { w :: Number, h :: Number }

foreign import dimensions :: forall eff. Eff (dom :: DOM | eff) Dimensions

dimensionsS  :: forall eff. Eff (dom :: DOM, timer :: Timer | eff) (Signal Dimensions)
dimensionsS = unwrap $ every second ~> \_ -> dimensions

main = do
  mouse <- mousePos
  dims  <- dimensionsS
  runSignal (render <~ rudolf mouse dims)
  where
  rudolf :: Signal CoordinatePair -> Signal Dimensions -> Signal CoordinatePair
  rudolf = zip position

  position :: CoordinatePair -> Dimensions -> CoordinatePair
  position o d = { x: d.w - o.x, y: d.h - o.y }
-}
