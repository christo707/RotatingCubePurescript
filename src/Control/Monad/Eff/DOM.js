"use strict";

var xxx = 0;
var yyy = 0;



exports.getMousePos = function(String){
  return function(){
    if(String == 'x')
     return xxx;
    else return yyy;
  };
};

exports.querySelectorImpl = function(r, f, s) {
    return function() {
        var result = document.querySelector(s);
        return result ? f(result) : r;
    };
};

exports.addEventListener = function(name) {
    return function(handler) {
        return function(node) {
            return function() {
                node.addEventListener(name, function(e) {
                    handler();
                    xxx = e.pageX;
                    yyy = e.pageY;
                    e.preventDefault();
                });
            };
        };
    };
};
