local wibox = require("wibox")
local naughty = require("naughty")

-- Return true if the battery is charging
function batteryCharging(adapter)
  local pipe = io.open("/sys/class/power_supply/" .. adapter .. "/status")
  local symbol = false
  if pipe then
    local status = pipe:read()
    pipe:close()
    if status:match("Charging") then
      symbol = true
    end
  end

  return symbol
end

-- Get the battery percentage
function batteryPercentage(adapter)
  local pipe = io.open("/sys/class/power_supply/" .. adapter .. "/capacity")
  local percent = "Unknown"
  if pipe then
    percent = pipe:read()
    pipe:close()
  end

  return percent
end

-- Set the wiget text and notify
function batteryInfo(widget, adapter)
  widget:set_text(" " .. batteryPercentage(adapter) .. "% ")
  notify(adapter)
end

-- Present a notification if the battery isn't charging and
--  has less than 10% battery
function notify(adapter)
  local status = batteryCharging(adapter)
  local percent = batteryPercentage(adapter)
  if not status and tonumber(percent) < 10 then
    naughty.notify({  preset  = naughty.config.presets.critical,
                      title   = "Battery Low",
                      text    = " " .. percent .. "% left",
                      timeout = 20,
                    });
  end
end

-- Setup the widget with timers
function batterySetup(adapter)
  widget = wibox.widget.textbox()
  batteryInfo(widget, adapter)
  aTimer = timer({ timeout = 30 })
  aTimer:connect_signal("timeout", function () batteryInfo(widget, adapter) end)
  aTimer:start()
  return widget
end

