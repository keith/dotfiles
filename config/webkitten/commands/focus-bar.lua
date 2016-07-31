function description()
  return "Focuses the command bar"
end

function run()
  focus_commandbar_in_window(focused_window_index())
  return false
end
