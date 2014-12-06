function jump(anchor) {
  var url = location.href;
  location.href = "#" + anchor;
}

$(document).keypress(function(e) {
  switch (e.which) {
    // e
    case 101:
      jump("Evolution");
      break;
    // t
    case 116:
      jump("Type_effectiveness");
      break;
    // w
    case 119:
      jump("Game_locations");
      break;
  }
});
