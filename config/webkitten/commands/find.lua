function description()
  return "Find text in the current webview"
end


function run()
  if #arguments > 0 then
    local windex = focused_window_index()
    local query = table.concat(arguments, " ")
    find(windex, focused_webview_index(windex), query)
  end
  return false -- continue to display command
end
