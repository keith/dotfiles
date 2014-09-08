$(document).ready(function() {
  setTimeout(function() {
    var button = $("a.js-nav-week")[0];
    if (button) {
      button.click();
    }
  }, 500);

  setTimeout(function() {
    var link = $("a.js-copy-over-rows-from-most-recent-timesheet")[0];
    if (link) {
      link.click();
    }
  }, 500);
});
