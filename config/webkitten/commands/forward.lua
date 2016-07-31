function run()
  local windex = focused_window_index()
  go_forward(windex, focused_webview_index(windex))
  return true
end
