-- This script's purpose
function description()
  return "Search DuckDuckGo"
end

-- Run default function. The current scope should include `arguments` as a
-- function which returns relevant options. Should return a boolean indicating
-- success or failure
function run()
  local windex = focused_window_index()
  local query = url_encode(table.concat(arguments, " "))
  local target = table.concat({"https://duckduckgo.com?q=", query}, "")
  load_uri(windex, focused_webview_index(windex), target)
  return true
end

function url_encode(str)
  if (str) then
  str = string.gsub (str, "\n", "\r\n")
  str = string.gsub (str, "([^%w %-%_%.%~])",
    function (c) return string.format ("%%%02X", string.byte(c)) end)
  str = string.gsub (str, " ", "+")
  end
  return str
end
