function description()
  return "Copy the URL"
end

function run()
  local windex = focused_window_index()
  local uri = webview_uri(windex, focused_webview_index(windex))
  set_command_field_text(windex, uri)
  focus_commandbar_in_window(windex)
  return false
end
