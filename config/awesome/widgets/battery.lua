local naughty = require("naughty")
local wibox = require("wibox")

-- These are all the battery states I observed in the 'status' file 'Full'
-- happens when the charger is plugged in *and* the battery is full
function batteryCharging(adapter)
  local pipe = io.open("/sys/class/power_supply/" .. adapter .. "/status")
  if not pipe then
    return false
  end

  local status = pipe:read()
  pipe:close()
  if status:match("Discharging") then
    return false
  elseif status:match("Charging") then
    return true
  elseif status:match("Full") then
    return true
  end

  return false
end

-- Previous this just cat'd the 'capacity' file. This file represents the
-- current battery capacity relative to the *original* maximum capacity (also
-- found in 'charge_full_design'). This new method compares what is now
-- considered the 'charge_full' amount, and the 'charge_now'. Not so
-- surprisingly 'charge_now' can exceed what is considered full, because of this
-- we max the value with 100, just to show a sane amount in the UI.
function batteryPercentage(adapter)
  local nowPipe = io.open("/sys/class/power_supply/" .. adapter .. "/charge_now")
  local fullPipe = io.open("/sys/class/power_supply/" .. adapter .. "/charge_full")
  local percent = "Unknown"
  if nowPipe and fullPipe then
    local now = nowPipe:read()
    local full = fullPipe:read()
    local charge = (now / full)

    percent = "" .. math.floor(math.min(charge * 100, 100))

    nowPipe:close()
    fullPipe:close()
  end

  return percent
end

-- Set the widget text and notify
function batteryInfo(widget, adapter)
  local charging = batteryCharging(adapter)
  local percent = batteryPercentage(adapter)
  widget:set_text(" " .. percent .. (charging and "+ " or "% "))
  notify(adapter)
end

-- Present a notification if the battery isn't charging and
-- has less than 10% battery
function notify(adapter)
  local status = batteryCharging(adapter)
  local percent = tonumber(batteryPercentage(adapter))
  if not status and (not percent or percent < 10) then
    naughty.notify({
      preset  = naughty.config.presets.critical,
      title   = "Battery Low",
      text    = " " .. percent .. "% left",
      timeout = 10,
    })
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
