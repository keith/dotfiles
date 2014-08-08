function alternate(URL) {
  var newURL = null;
  if (URL.endsWith(".h")) {
    newURL = URL.replaceLast("m");
  } else if (URL.endsWith(".m")) {
    newURL = URL.replaceLast("h");
  }

  if (newURL) {
    window.location.href = newURL;
  }
};

String.prototype.endsWith = function(suffix) {
  return this.indexOf(suffix, this.length - suffix.length) !== -1;
};

String.prototype.replaceLast = function(character) {
  return this.substr(0, this.length - 1) + character;
};

$(document).keypress(function(e) {
  var aPressed = (e.which == 97 ? true : false);
  if (aPressed) {
    alternate(window.location.href);
  }
});
