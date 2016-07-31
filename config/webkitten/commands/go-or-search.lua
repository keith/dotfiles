function description()
  return "Open a passed URL or searches for it on DuckDuckGo"
end

function run()
  local windex = focused_window_index()
  local input = table.concat(arguments, " ")
  local target = ""

  if looks_like_url(input) then
    target = input
  else
    local encoded = url_encode(input)
    target = table.concat({"https://duckduckgo.com?q=", encoded}, "")
  end

  load_uri(windex, focused_webview_index(windex), target)

  return true
end

function looks_like_url(str)
  -- Anything with a space in it is treat it as a search
  -- EX: "foo bar"
  if str:find(" ") then
    return false
  end

  -- If it doesn't have a dot, treat it as a search
  -- EX: "foo"
  if str:find("%.") == nil then
    return false
  end

  -- If it starts with a question mark, treat it as a search
  -- EX: "?google.com"
  if str:find("^?") then
    return false
  end

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
