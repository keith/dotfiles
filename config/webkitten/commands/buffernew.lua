function description()
  return "Opens a new buffer with a URL or configured start page"
end

function run()
  local target = ""
  local windex = focused_window_index()
  if #arguments > 0 then
    target = arguments[1]
  else
    target = lookup_string(config_file_path, "window.start-page")
  end

  if windex ~= NOT_FOUND then
    open_webview(windex, target)
  else
    windex = open_window(target)
  end

  focus_commandbar_in_window(windex)
  return true
end
