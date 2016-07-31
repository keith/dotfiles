function run()
  local windex = focused_window_index()
  if webview_count(windex) > 1 then
    close_webview(windex, focused_webview_index(windex))
  else
    close_window(windex)
  end
  return true
end

