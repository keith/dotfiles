// Keith's [Conductor](https://github.com/keith/conductor) config
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

Window.fullScreen = function() { toGrid(0, 0, 1, 1); }
Window.leftHalf = function() { toGrid(0, 0, 0.5, 1); }
Window.rightHalf = function() { toGrid(0.5, 0, 0.5, 1); }
Window.topLeft = function() { toGrid(0, 0, 0.5, 0.5); }
Window.bottomLeft = function() { toGrid(0, 0.5, 0.5, 0.5); }
Window.topRight = function() { toGrid(0.5, 0, 0.5, 0.5); }
Window.bottomRight = function() { toGrid(0.5, 0.5, 0.5, 0.5); }

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

function toggleAppIfOpen(title) {
  var app = App.frontmostApp();
  if (app.title() === title) {
    app.hide();
  } else {
    var apps = App.runningApps().map(function(app) {
      return app.title();
    });

    if (apps.indexOf(title) >= 0) {
      api.launch(title);
    }
  }
}

function toggleApp(title) {
  var app = App.frontmostApp();
  if (app.title() === title) {
    app.hide();
  } else {
    api.launch(title);
  }
}

var modifiers = ["ctrl", "cmd"];
api.bind('u', modifiers, center);
api.bind('i', modifiers, left);
api.bind('o', modifiers, right);
api.bind('p', modifiers, push);

api.bind('k', modifiers, Window.fullScreen);
api.bind('h', modifiers, Window.leftHalf);
api.bind('l', modifiers, Window.rightHalf);

api.bind('n', modifiers, Window.topLeft);
api.bind('m', modifiers, Window.bottomLeft);
api.bind(',', modifiers, Window.topRight);
api.bind('.', modifiers, Window.bottomRight);

api.bind('F1', [], function() { Screen.setBrightness(Screen.getBrightness() - 6.25); });
api.bind('F2', [], function() { Screen.setBrightness(Screen.getBrightness() + 6.25); });

api.bind('RETURN', ["shift", "cmd"], function() { toggleApp("Messages"); });
api.bind('d', ["ctrl", "alt"], function() { toggleApp("Dash"); });
api.bind('a', ["shift", "alt", "cmd"], function() { api.launch("Activity Monitor"); });

Config.hideMenuBar();
