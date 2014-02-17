// Keith's [Phoenix](https://github.com/sdegutis/Phoenix) config

var modifiers = ["ctrl", "cmd"];
var border = 2;

function fullScreen() {
  var win = Window.focusedWindow();
  var sframe = win.screen().frameIncludingDockAndMenu();
  win.setFrame({
    x: sframe.x,
    y: sframe.y,
    width: sframe.width,
    height: sframe.height
  });
}

function halfSize() {
  var win = Window.focusedWindow();
  var sframe = win.screen().frameIncludingDockAndMenu();
  var wframe = win.frame();
  var originY = sframe.y + (sframe.height - (wframe.height / 2)) / 2;
  win.setFrame({
    x: sframe.x,
    y: originY,
    width: wframe.width / 2,
    height: wframe.height / 2
  });
}

function leftHalf() {
  var win = Window.focusedWindow();
  var sframe = win.screen().frameIncludingDockAndMenu();
  var width =
  win.setFrame({
    x: sframe.x,
    y: sframe.y,
    width: sframe.width / 2 - border,
    height: sframe.height
  });
}

function rightHalf() {
  var win = Window.focusedWindow();
  var sframe = win.screen().frameIncludingDockAndMenu();
  win.setFrame({
    x: sframe.width / 2 + border,
    y: sframe.y,
    width: sframe.width / 2,
    height: sframe.height
  });
}

function topHalf() {
  var win = Window.focusedWindow();
  var sframe = win.screen().frameWithoutDockOrMenu();
  win.setFrame({
    x: sframe.x,
    y: sframe.y,
    width: sframe.width,
    // 18 seems to correct issues with the height because of the given frame?
    height: sframe.height / 2 - 18
  });
}

function bottomHalf() {
  var win = Window.focusedWindow();
  var sframe = win.screen().frameIncludingDockAndMenu();
  win.setFrame({
    x: sframe.x,
    y: sframe.height / 2,
    width: sframe.width,
    height: sframe.height / 2 - border
  });
}

function topLeft() {
  var win = Window.focusedWindow();
  var sframe = win.screen().frameWithoutDockOrMenu();
  win.setFrame({
    x: sframe.x,
    y: sframe.y,
    width: sframe.width / 2 - border,
    height: sframe.height / 2 - border
  });
}

function bottomLeft() {
  var win = Window.focusedWindow();
  var sframe = win.screen().frameWithoutDockOrMenu();
  win.setFrame({
    x: sframe.x,
    y: sframe.y + sframe.height / 2 + border,
    width: sframe.width / 2 - border,
    height: sframe.height / 2 - border
  });
}

function topRight() {
  var win = Window.focusedWindow();
  var sframe = win.screen().frameWithoutDockOrMenu();
  win.setFrame({
    x: sframe.x + sframe.width / 2 + border,
    y: sframe.y,
    width: sframe.width / 2 - border,
    height: sframe.height / 2 - border
  });
}

function bottomRight() {
  var win = Window.focusedWindow();
  var sframe = win.screen().frameWithoutDockOrMenu();
  win.setFrame({
    x: sframe.x + sframe.width / 2 + border,
    y: sframe.y + sframe.height / 2,
    width: sframe.width / 2 - border,
    height: sframe.height / 2 - border
  });
}

api.bind('k', modifiers, function() { fullScreen() });
api.bind('j', modifiers, function() { halfSize() });

api.bind('h', modifiers, function() { leftHalf() });
api.bind('l', modifiers, function() { rightHalf() });
api.bind('i', modifiers, function() { topHalf() });
api.bind('u', modifiers, function() { bottomHalf() });

api.bind('n', modifiers, function() { topLeft() });
api.bind('m', modifiers, function() { bottomLeft() });
api.bind(',', modifiers, function() { topRight() });
api.bind('.', modifiers, function() { bottomRight() });

var moveModifiers = ["ctrl", "alt"];
var resizeModifiers = ["ctrl", "alt", "cmd"];

function nudge(x, y) {
  var win = Window.focusedWindow();
  var wframe = win.frame();
  var xPercent = wframe.width * (x / 100);
  var yPercent = wframe.height * (y / 100);
  win.setFrame({
    x: wframe.x + xPercent,
    y: wframe.y + yPercent,
    width: wframe.width,
    height: wframe.height
  });
}

function resize(w, h) {
  var win = Window.focusedWindow();
  var wframe = win.frame();
  var wPercent = wframe.width * (w / 100);
  var hPercent = wframe.height * (h / 100);
  win.setFrame({
    x: wframe.x,
    y: wframe.y,
    width: wframe.width + wPercent,
    height: wframe.height + hPercent
  });
}

api.bind('left', moveModifiers, function() { nudge(-5, 0) });
api.bind('right', moveModifiers, function() { nudge(+5, 0) });
api.bind('up', moveModifiers, function() { nudge(0, -5) });
api.bind('down', moveModifiers, function() { nudge(0, +5) });

api.bind('left', resizeModifiers, function() { resize(-10, 0) });
api.bind('right', resizeModifiers, function() { resize(+10, 0) });
api.bind('up', resizeModifiers, function() { resize(0, -10) });
api.bind('down', resizeModifiers, function() { resize(0, +10) });
