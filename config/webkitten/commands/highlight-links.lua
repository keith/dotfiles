function description()
  return "Highlights and opens links without a mouse"
end

function run()
  local windex = focused_window_index()
  local webdex = focused_webview_index(windex)
  run_javascript(windex, webdex, [[
    var links = document.getElementsByTagName("a"),
      target = [], timer = null,
      ESC_CODE = 27, ENTER_CODE = 10, ZERO_CODE = 48, NINE_CODE = 57;
    for (var i = 0; i < links.length; i++) {
      var span = document.createElement("span");
      span.className = "hlink";
      span.innerText = "" + i;
      span.setAttribute("style","color: #888; vertical-align: top; border: 1px solid #ccc; border-radius: 2px; background: yellow; font-family: sans-serif; font-size: 11px");
      links[i].insertAdjacentElement("afterbegin", span);
    }
    document.onkeydown = function(event) {
      var clickFunc = function() {
        if (document.activeElement && document.activeElement.tagName != "BODY") {
          return; // don't perform action from input areas
        }
        var selected = links[parseInt(target.join(""))];
        if (selected) {
          selected.click();
        }
        target = [];
        if (timer) {
          window.clearTimeout(timer);
        }
      };
      if (event.keyCode == ENTER_CODE) {
        clickFunc();
      } else if (event.keyCode >= ZERO_CODE && event.keyCode <= NINE_CODE) {
        target.push(String.fromCharCode(event.keyCode));
        if (timer) {
          window.clearTimeout(timer);
        }
        timer = window.setTimeout(clickFunc, 800);
      } else if (event.keyCode == ESC_CODE) {
        target = [];
        var spans = document.querySelectorAll("span.hlink");
        for (var i = 0; i < spans.length; i++) {
          spans[i].remove();
        }
        if (timer) {
          window.clearTimeout(timer);
        }
        document.onkeydown = null;
      }
    };
  ]])
  focus_webview_in_window(windex)
  return true
end
