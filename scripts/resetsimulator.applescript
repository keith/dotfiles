tell application "System Events" to tell process "iOS Simulator"
	activate
	set frontmost to true
	click menu item "Reset Content and Settings…" of menu 0 of menu bar item "iOS Simulator" of menu bar 1
	click button "Reset" of window ""
  return
end tell
