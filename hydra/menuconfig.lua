dofile(package.searchpath("updateconfig", package.path))

-- show a helpful menu
hydra.menu.show(function()
  return {
    {title = "Reload Config", fn = hydra.reload},
    {title = "-"},
    {title = "About", fn = hydra.showabout},
    {title = "Check for Updates...", fn = function() hydra.updates.check(nil, true) end},
    {title = "Quit", fn = os.exit},
  }
end)
