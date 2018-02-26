module Cube where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.DOM (querySelector, addEventListener, getMousePos)
import Control.Monad.Eff.Ref (REF, newRef, readRef, modifyRef, Ref)
import Control.Monad.Eff.Timer as T
import DOM (DOM)
import DOM.Event.MouseEvent (MouseEvent, eventToMouseEvent, mouseEventToEvent, pageX)
import DOM.Event.Types (Event, EventType(..), MouseEvent)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (mousedown)
import DOM.HTML.Types (WINDOW)
import DOM.HTML.Window (screenX, screenY)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import FFI.Util (setProperty, property)
import Graphics.Canvas (CANVAS, CanvasElement, Context2D, translate, arc, clearRect, closePath, fillPath, fillText, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, lineTo, moveTo, setFillStyle, setStrokeStyle, strokePath)
import Math (pi, cos, sin)
import Partial.Unsafe (unsafePartial)

newtype Point3D = Point3D
  { x :: Number
  , y :: Number
  , z :: Number
  }

newtype Point2D = Point2D
  { x :: Number
  , y :: Number
  }

newtype Angle3D = Angle3D
  { qx :: Number
  , qy :: Number
  , qz :: Number
}

type Postion =  {x :: Int, y :: Int}

half:: Number
half = 100.0

project :: Point3D -> Angle3D -> Point2D
project (Point3D { x, y, z }) (Angle3D { qx, qy, qz }) =
  let xByz = x * (cos qz) + y * (sin qz)
      yByz = y * (cos qz) - x * (sin qz)
      yByzByx = yByz * (cos qx) + z * (sin qx)
      zByzByx = z * (cos qx) - yByz * (sin qx)
      xByzByxByy = xByz * (cos qy) + zByzByx * (sin qy)
   in
     Point2D { x: xByzByxByy, y: yByzByx }

  {-let yByx = y * (cos qx) - z * (sin qx)
      zByx = z * (cos qx) + y * (sin qx)
      xByxByy = x * (cos qy) - zByx * (sin qy)
      zByxByy = zByx * (cos qy) - x * (sin qy)
  in
   Point2D { x: xByxByy, y: yByx }
-}
drawLine :: forall e. Context2D -> Point2D -> Point2D -> Eff (canvas :: CANVAS | e) Unit
drawLine ctx (Point2D from) (Point2D to) = strokePath ctx $ do
   _ <- setStrokeStyle "#0000FF" ctx
   _ <- moveTo ctx from.x from.y
   _ <- lineTo ctx to.x to.y
   _ <- closePath ctx
   pure unit

canvasClean :: forall e. CanvasElement -> Eff (canvas :: CANVAS | e) Unit
canvasClean canvas = do
  width <- getCanvasWidth canvas
  height <- getCanvasHeight canvas
  ctx <- getContext2D canvas
  void $ clearRect ctx { x: 0.0 - width, y: 0.0 - height, w: 2.0 * width, h: 2.0 * height }

drawCircle :: forall e. Context2D -> Point2D -> String -> Eff (canvas :: CANVAS | e) Unit
drawCircle ctx (Point2D at) no= fillPath ctx $ do
  _ <- setFillStyle "#000000" ctx
  _ <- fillText ctx no (at.x + 5.0) (at.y - 4.0)
  _ <- setFillStyle "#FF0000" ctx
  _ <- arc ctx
       { x      : at.x
       , y      : at.y
       , r      : 4.0
       , start  : 0.0
       , end    : pi * 2.0
       }
  pure unit

drawCube :: forall e . Context2D -> Ref Boolean -> Ref Number -> Ref Number -> Ref Number-> Ref Number-> Ref Number -> Ref Number -> Ref Number-> Angle3D -> Eff (canvas :: CANVAS,console :: CONSOLE, dom :: DOM, ref :: REF, timer :: T.TIMER| e) Unit
drawCube ctx drag old_x old_y dX dY alpha beta gamma (Angle3D { qx, qy, qz })= void $ unsafePartial do
   Just canvas <- getCanvasElementById "canvas"
   canvasClean canvas
   let v1 = project (Point3D { x: 0.0 - half, y: 0.0 - half, z: 0.0 - half }) (Angle3D {qx: qx, qy: qy, qz: qz})
   drawCircle ctx v1 "8"
   let v2 = project (Point3D { x: 0.0 - half, y: 0.0 + half, z: 0.0 - half }) (Angle3D {qx: qx, qy: qy, qz: qz})
   drawCircle ctx v2 "7"
   let v3 = project (Point3D { x: 0.0 - half, y: 0.0 - half, z: 0.0 + half }) (Angle3D {qx: qx, qy: qy, qz: qz})
   drawCircle ctx v3 "1"
   let v4 = project (Point3D { x: 0.0 - half, y: 0.0 + half, z: 0.0 + half }) (Angle3D {qx: qx, qy: qy, qz: qz})
   drawCircle ctx v4 "2"
   let v5 = project (Point3D { x: 0.0 + half, y: 0.0 - half, z: 0.0 - half }) (Angle3D {qx: qx, qy: qy, qz: qz})
   drawCircle ctx v5 "5"
   let v6 = project (Point3D { x: 0.0 + half, y: 0.0 + half, z: 0.0 - half }) (Angle3D {qx: qx, qy: qy, qz: qz})
   drawCircle ctx v6 "6"
   let v7 = project (Point3D { x: 0.0 + half, y: 0.0 - half, z: 0.0 + half }) (Angle3D {qx: qx, qy: qy, qz: qz})
   drawCircle ctx v7 "4"
   let v8 = project (Point3D { x: 0.0 + half, y: 0.0 + half, z: 0.0 + half }) (Angle3D {qx: qx, qy: qy, qz: qz})
   drawCircle ctx v8 "3"

   drawLine ctx v1 v5
   drawLine ctx v5 v6
   drawLine ctx v6 v2
   drawLine ctx v2 v1

   drawLine ctx v3 v7
   drawLine ctx v7 v8
   drawLine ctx v8 v4
   drawLine ctx v4 v3

   drawLine ctx v1 v3
   drawLine ctx v5 v7
   drawLine ctx v6 v8
   drawLine ctx v2 v4


   node <- querySelector "#canvas"

   --addEventListener canvas "mousedown" mouseDown
   for_ node $ addEventListener "mousedown" $ void  do
     modifyRef drag \d -> true
     --xx <- pageX (eventToMouseEvent (mouseEventToEvent MouseEvent))
     --x <- liftEff $ screenX =<< window
     --y <- liftEff $ screenY =<< window
     x <- (getMousePos "x")
     y <- getMousePos "y"
     modifyRef old_x \ox ->  toNumber x
     modifyRef old_y \oy ->  toNumber y
   for_ node $ addEventListener "mouseup" $ void do
     modifyRef drag \d -> false
   for_ node $ addEventListener "mouseout" $ void  do
     modifyRef drag \d -> false

   for_ node $ addEventListener "mousemove" $ void do
     --log "Mouse Moved!"
     --x <- liftEff $ screenX =<< window
     --y <- liftEff $ screenY =<< window
     x <- getMousePos "x"
     y <- getMousePos "y"
     ox <- readRef old_x
     oy <- readRef old_y
     modifyRef dX \dy -> (toNumber y - oy) * 4.0 * pi / 650.0
     modifyRef dY \dx -> (toNumber x - ox) * 4.0 * pi / 600.0
     dx <- readRef dX
     dy <- readRef dY
     dg <- readRef drag
     if dg == true then do
      modifyRef alpha \al -> al + dx
      modifyRef beta \bt -> bt + dy
      modifyRef old_x \ox -> toNumber x
      modifyRef old_y \oy -> toNumber y
      --modifyRef gamma \ga -> ga + 3.0 * pi/180.0
      else
        pure unit

   dg <- readRef drag
   if dg == false then do
    modifyRef dX \dx -> dx * 0.95
    modifyRef dY \dy -> dy * 0.95
    dx <- readRef dX
    dy <- readRef dY
    modifyRef alpha \al -> al * 0.95
    modifyRef beta \bt -> bt * 0.95
    --modifyRef gamma \ga -> ga * 0.90
    else
      pure unit

   al <- readRef alpha
   bt <- readRef beta
   ga <- readRef gamma
   void $ T.setTimeout 30 do
      drawCube ctx drag old_x old_y dX dY alpha beta gamma (Angle3D{qx: al , qy: bt, qz: ga})

main :: forall e.Eff(canvas :: CANVAS,window :: WINDOW, console :: CONSOLE, dom :: DOM, ref :: REF, timer :: T.TIMER | e) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  alpha <- newRef 0.0
  beta <- newRef 0.0
  gamma <- newRef 0.0
  dX <- newRef 0.0
  dY <- newRef 0.0
  drag <- newRef false
  old_x <- newRef 0.0
  old_y <- newRef 0.0
  (w :: Number) <- getCanvasWidth canvas
  (h :: Number ) <- getCanvasHeight canvas
  _ <- translate ({translateX : w/2.0, translateY: h/2.0}) ctx

    --modifyRef gamma \ga -> ga + 1.0 * pi/180.0
  drawCube ctx drag old_x old_y dX dY alpha beta gamma (Angle3D{qx: 0.0, qy: 0.0, qz: 0.0})
