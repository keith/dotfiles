// Keith's [Phoenix](https://github.com/Keithbsmiley/phoenix) config

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

// function bottomHalf() {
//   var win = Window.focusedWindow();
//   var sframe = win.screen().frameIncludingDockAndMenu();
//   win.setFrame({
//     x: sframe.x,
//     y: sframe.height / 2,
//     width: sframe.width,
//     height: sframe.height / 2 - border
//   });
// }

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

function center() {
  var win = Window.focusedWindow();
  var sframe = win.screen().frameWithoutDockOrMenu();
  var frame = win.frame();
  frame.x = sframe.x + ((sframe.width / 2) - (frame.width / 2));
  frame.x = sframe.x + ((sframe.width / 2) - (frame.width / 2));
  win.setFrame(frame);
}

function push() {
  var win = Window.focusedWindow();
  var frame = win.frame();
  var nextScreen = win.screen().nextScreen();
  var screenFrame = nextScreen.frameWithoutDockOrMenu();
  win.setFrame({
    x: screenFrame.x,
    y: screenFrame.y,
    width: frame.width,
    height: frame.height
  });
}

api.bind('u', modifiers, function() { center() });
api.bind('p', modifiers, function() { push() });

api.bind('k', modifiers, function() { fullScreen() });
api.bind('j', modifiers, function() { halfSize() });

api.bind('h', modifiers, function() { leftHalf() });
api.bind('l', modifiers, function() { rightHalf() });
api.bind('i', modifiers, function() { topHalf() });

api.bind('n', modifiers, function() { topLeft() });
api.bind('m', modifiers, function() { bottomLeft() });
api.bind(',', modifiers, function() { topRight() });
api.bind('.', modifiers, function() { bottomRight() });

api.bind('F1', [], function() { Screen.setBrightness(Screen.getBrightness() - 6.25); });
api.bind('F2', [], function() { Screen.setBrightness(Screen.getBrightness() + 6.25); });

api.bind('y', modifiers, function () { api.launch("iTerm"); });
