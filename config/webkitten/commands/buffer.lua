function run()
  local windex = focused_window_index()
  if #arguments > 0 then
    local index = tonumber(arguments[1])
    if index > webview_count(windex) then
      return false
    end
    focus_webview(windex, tonumber(arguments[1]))
  end
  return true
end

-- Display titles for buffers
function complete_command()
  local titles = {}
  local query = table.concat(arguments," ")
  local windex = focused_window_index()
  local total = webview_count(windex)
  for i = 0, total - 1 do
    local title = "" .. i .. " : " .. webview_title(windex, i):gsub(","," ")
    if string.sub(title, 1, #query) == query then
      titles[#titles + 1] = title
    end
  end
  return table.concat(titles,",")
end
