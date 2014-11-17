function next(URL) {
  var index = URL.lastIndexOf("/") + 3;
  var path = URL.substring(index);
  var base = URL.substring(0, index);
  var next = parseInt(path.substring(0, path.lastIndexOf("."))) + 1;
  console.log("Hello" + index);

  var newURL = base + next + ".html";
  window.location.href = newURL;
};

$(document).keypress(function(e) {
  // The letter 'd'
  var aPressed = (e.which == 100 ? true : false);
  if (aPressed) {
    next(window.location.href);
  }
});
