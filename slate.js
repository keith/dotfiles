// Keith's [Slate](https://github.com/jigish/slate) configuration file
// A constant work in progresss

// I assume this means with multiple monitors it defaults to the one your cursor is on
S.cfga({
  "defaultToCurrentScreen" : true
});

var fullscreen = slate.operation("move", {
  "x" : "screenOriginX",
  "y" : "screenOriginY",
  "width" : "screenSizeX",
  "height" : "screenSizeY"
});

var halfsize = slate.operation("move", {
  "x" : "screenOriginX",
  "y" : "screenOriginY + (screenSizeY - (windowSizeY / 2)) / 2",
  "width" : "windowSizeX / 2",
  "height" : "windowSizeY / 2"
});

S.bind("k:ctrl;cmd", fullscreen);
S.bind("j:ctrl;cmd", halfsize);

S.bnda({
  // Moving Windows using Control+Alt and the Arrow keys
  "left:ctrl;alt" : S.op("nudge", { "x" : "-5%", "y" : "+0" }),
  "right:ctrl;alt" : S.op("nudge", { "x" : "+5%", "y" : "+0" }),
  "up:ctrl;alt" : S.op("nudge", { "x" : "+0", "y" : "-5%" }),
  "down:ctrl;alt" : S.op("nudge", { "x" : "+0", "y" : "+5%" }),

  // Resizing Windows by the bottom right using Control+Alt+Cmd and they arrow or vim keys
  "left:ctrl;alt;cmd" : S.op("resize", { "width" : "-10%", "height" : "+0" }),
  "right:ctrl;alt;cmd" : S.op("resize", { "width" : "+10%", "height" : "+0" }),
  "up:ctrl;alt;cmd" : S.op("resize", { "width" : "+0", "height" : "-10%" }),
  "down:ctrl;alt;cmd" : S.op("resize", { "width" : "+0", "height" : "+10%" }),
});

S.log("[SLATE] -------------- Finished Loading Config --------------");

