function description()
  return "Opens a new window"
end

function run()
  local windex = open_window("")
  focus_commandbar_in_window(windex)
  return true
end
