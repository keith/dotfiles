// Keith's [Phoenix](https://github.com/keith/phoenix) config
var modifiers = ["ctrl", "cmd"];
var padding = 2;

function windowToGrid(win, x, y, width, height) {
  var screen = win.screen().frameIncludingDockAndMenu();

  win.setFrame({
    x: Math.round(x * screen.width) + padding + screen.x,
    y: Math.round(y * screen.height) + padding + screen.y,
    width: Math.round(width * screen.width) - (2 * padding),
    height: Math.round(height * screen.height) - (2 * padding)
  });
}

function toGrid(x, y, width, height) {
  windowToGrid(Window.focusedWindow(), x, y, width, height);
}

Window.fullScreen = function() {
  toGrid(0, 0, 1, 1);
}

Window.leftHalf = function() {
  toGrid(0, 0, 0.5, 1);
}

Window.rightHalf = function() {
  toGrid(0.5, 0, 0.5, 1);
}

Window.topLeft = function() {
  toGrid(0, 0, 0.5, 0.5);
}

Window.bottomLeft = function() {
  toGrid(0, 0.5, 0.5, 0.5);
}

Window.topRight = function() {
  toGrid(0.5, 0, 0.5, 0.5);
}

Window.bottomRight = function() {
  toGrid(0.5, 0.5, 0.5, 0.5);
}

function center() {
  var win = Window.focusedWindow();
  var sframe = win.screen().frameWithoutDockOrMenu();
  var frame = win.frame();
  frame.x = sframe.x + ((sframe.width / 2) - (frame.width / 2));
  frame.y = sframe.y + ((sframe.height / 2) - (frame.height / 2));
  win.setFrame(frame);
}

function left() {
  var win = Window.focusedWindow();
  var frame = win.frame();
  frame.x = 0;
  win.setFrame(frame);
}

function right() {
  var win = Window.focusedWindow();
  var sframe = win.screen().frameWithoutDockOrMenu();
  var frame = win.frame();
  frame.x = sframe.width - frame.width;
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
api.bind('i', modifiers, function() { left() });
api.bind('o', modifiers, function() { right() });
api.bind('p', modifiers, function() { push() });

api.bind('k', modifiers, function() { Window.fullScreen() });
api.bind('h', modifiers, function() { Window.leftHalf() });
api.bind('l', modifiers, function() { Window.rightHalf() });

api.bind('n', modifiers, function() { Window.topLeft() });
api.bind('m', modifiers, function() { Window.bottomLeft() });
api.bind(',', modifiers, function() { Window.topRight() });
api.bind('.', modifiers, function() { Window.bottomRight() });

api.bind('F1', [], function() { Screen.setBrightness(Screen.getBrightness() - 6.25); });
api.bind('F2', [], function() { Screen.setBrightness(Screen.getBrightness() + 6.25); });

api.bind('y', ["ctrl", "alt"], function () { api.launch("iTerm"); });
api.bind('RETURN', ["shift", "cmd"], function () { api.launch("Messages"); });
