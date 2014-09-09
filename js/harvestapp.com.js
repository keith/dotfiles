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

  // setTimeout(function() {
  //   var rowChildren = $("div.task:contains('Development')").closest("tr").children()
  //   var days = rowChildren.filter("td.day");
  //   var weekDays = days.slice(0, -3);
  //   weekDays.each(function(index, value) {
  //     var child = value.children[0];
  //     child.value = "8:00";
  //   });
  // }, 500);
});
