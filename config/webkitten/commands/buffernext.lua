function run()
  local windex = focused_window_index()
  local target = focused_webview_index(windex) + 1
  if target >= webview_count(windex) then
    target = 0
  end
  focus_webview(windex, target)
  return true
end
