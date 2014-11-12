function appendParameter(param) {
  var URL = window.location.href;
  if (URL.contains("?")) {
    window.location.href += "&" + param;
  } else {
    window.location.href = "?" + param;
  }
}

$(document).ready(function() {
  var URL = window.location.href;
  var param = "share=1";
  if (!URL.contains(param)) {
    appendParameter(param);
  }
});
