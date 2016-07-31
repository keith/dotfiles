function description()
  return "Open a web page"
end

function run()
  if #arguments > 0 then
    local windex = focused_window_index()
    local target = arguments[1]
    load_uri(windex, focused_webview_index(windex), target)
    focus_webview_in_window(windex)
    return true
  end
  log_debug("No URL specified")
  return false
end
