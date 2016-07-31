function description()
  return "Highlights and focuses inputs/buttons without a mouse"
end

function run()
  local windex = focused_window_index()
  local webdex = focused_webview_index(windex)
  run_javascript(windex, webdex, [[
    var inputs = document.querySelectorAll("input,button"),
      target = [], timer = null,
      ESC_CODE = 27, ENTER_CODE = 10, ZERO_CODE = 48, NINE_CODE = 57;
    for (var i = 0; i < inputs.length; i++) {
      var span = document.createElement("span");
      span.className = "hinput";
      span.innerText = "" + i;
      span.setAttribute("style","color: #888; vertical-align: bottom; border: 1px solid #ccc; border-radius: 2px; background: DEECFA; font-family: sans-serif; font-size: 11px");
      inputs[i].parentElement.insertBefore(span, inputs[i]);
    }
    document.onkeydown = function(event) {
      var clearFunc = function() {
        target = [];
        var spans = document.querySelectorAll("span.hinput");
        for (var i = 0; i < spans.length; i++) {
          spans[i].remove();
        }
        if (timer) {
          window.clearTimeout(timer);
        }
        document.onkeydown = null;
      };
      var focusFunc = function() {
        if (document.activeElement && document.activeElement.tagName != "BODY") {
          return; // don't perform action from input areas
        }
        var selected = inputs[parseInt(target.join(""))];
        if (selected) {
          selected.focus();
        }
        clearFunc();
      };
      if (event.keyCode == ENTER_CODE) {
        focusFunc();
      } else if (event.keyCode >= ZERO_CODE && event.keyCode <= NINE_CODE) {
        target.push(String.fromCharCode(event.keyCode));
        if (timer) {
          window.clearTimeout(timer);
        }
        timer = window.setTimeout(focusFunc, 800);
      } else if (event.keyCode == ESC_CODE) {
        clearFunc();
      }
    };
  ]])
  focus_webview_in_window(windex)
  return true
end
