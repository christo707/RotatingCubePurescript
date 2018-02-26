"use strict";



exports.getX = function(event) {
  return event.pageX;
}
/*

exports.getY = function(event) {
  return event.pageY;
}

exports.moused = function(ctx){
   return function(event){
    return function(callback){

    function mousedown(e){
      callback(e)();
    }
    ctx.addEventListener(event, mousedown);
    return funtion(){}
   }
}
}

foreign import addEventListener :: CanvasElement -> String -> (Event -> Eff (canvas :: CANVAS, console :: CONSOLE) Unit ) -> (Eff (canvas :: CANVAS, console :: CONSOLE ) Unit )

mouseDown :: forall e. Event -> Eff (canvas :: CANVAS, console :: CONSOLE | e) Unit
mouseDown  e = do
   let (x :: Number ) = property e "offSetX"
   let (y :: Number ) = property e "offSetY"
   log (show x)
   pure unit
*/
