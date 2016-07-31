function run()
  local windex = focused_window_index()
  local target = focused_webview_index(windex) - 1
  if target < 0 then
    target = webview_count(windex) - 1
  end
  focus_webview(windex, target)
  return true
end
