dofile(package.searchpath("grid", package.path))
dofile(package.searchpath("menuconfig", package.path))

hydra.alert("Hydra config loaded", 0.5)

pathwatcher.new(os.getenv("HOME") .. "/.hydra/", hydra.reload):start()
hydra.autolaunch.set(true)

local ctrlaltcmd = {"ctrl", "alt", "cmd"}
local ctrlcmd = {"ctrl", "cmd"}

hotkey.bind(ctrlcmd, 'K', ext.grid.fullscreen)
hotkey.bind(ctrlcmd, 'H', ext.grid.lefthalf)
hotkey.bind(ctrlcmd, 'L', ext.grid.righthalf)
hotkey.bind(ctrlcmd, 'P', ext.grid.pushwindow)
hotkey.bind(ctrlcmd, 'U', ext.grid.center)

hotkey.bind(ctrlcmd, 'N', ext.grid.topleft)
hotkey.bind(ctrlcmd, 'M', ext.grid.bottomleft)
hotkey.bind(ctrlcmd, ',', ext.grid.topright)
hotkey.bind(ctrlcmd, '.', ext.grid.bottomright)

hotkey.bind({"ctrl", "alt"}, 'Y', function() application.launchorfocus("iTerm") end)
hotkey.bind(ctrlaltcmd, 'R', function() repl.open(); logger.show() end)

hotkey.bind({}, 'F1', function() brightness.set(brightness.get() - 6.25) end)
hotkey.bind({}, 'F2', function() brightness.set(brightness.get() + 6.25) end)
