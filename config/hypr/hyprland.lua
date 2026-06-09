local mod = "SUPER"
local launcher = "wofi --show drun"

hl.monitor {
  output = "",
  mode = "preferred",
  position = "auto",
  scale = "auto",
}

hl.env("XCURSOR_SIZE", "24")
hl.env("HYPRCURSOR_SIZE", "24")

hl.config {
  general = {
    gaps_in = 6,
    gaps_out = 10,
    border_size = 2,
    resize_on_border = true,
    layout = "dwindle",
    col = {
      active_border = "rgb(5f87af)",
      inactive_border = "rgb(444444)",
    },
  },

  input = {
    kb_layout = "us",
    kb_options = "caps:escape",
    follow_mouse = 0,
    sensitivity = 0,
    touchpad = {
      natural_scroll = true,
      tap_to_click = true,
    },
  },

  decoration = {
    rounding = 6,
    active_opacity = 1.0,
    inactive_opacity = 1.0,
    shadow = {
      enabled = true,
      range = 8,
      render_power = 2,
      color = "rgba(00000044)",
    },
  },

  animations = {
    enabled = false,
  },

  misc = {
    disable_hyprland_logo = true,
    disable_autoreload = true,
    force_default_wallpaper = 0,
  },
}

hl.on("hyprland.start", function()
  hl.exec_cmd "hyprpm reload"
  hl.exec_cmd "keyd-application-mapper -d"
  hl.exec_cmd "mako"
  hl.exec_cmd "systemctl --user start hyprpolkitagent"
  hl.exec_cmd "waybar"

  -- Not sure if this needs to be last but feels right
  hl.exec_cmd "dbus-update-activation-environment --systemd --all"
end)

hl.window_rule {
  name = "float-all-windows",
  match = { class = ".*" },
  float = true,
}

hl.bind("CTRL + ALT + SUPER + M", hl.dsp.exec_cmd "hyprshutdown")
hl.bind("CTRL + ALT + SUPER + R", hl.dsp.exec_cmd "hyprctl reload && notify-send -t 2000 'Hyprland Reloaded'")
-- FIXME: laptop config
-- hl.bind("switch:on:Lid Switch", hl.dsp.exec_cmd "loginctl lock-session", { locked = true })

hl.bind(mod .. " + W", hl.dsp.window.close())
hl.bind("CTRL + " .. mod .. " + L", hl.dsp.exec_cmd "hypr-snap-window right")
hl.bind("CTRL + " .. mod .. " + H", hl.dsp.exec_cmd "hypr-snap-window left")
hl.bind("CTRL + " .. mod .. " + K", hl.dsp.exec_cmd "hypr-snap-window max")
hl.bind("CTRL + " .. mod .. " + F", hl.dsp.window.fullscreen { mode = "fullscreen", action = "toggle" })

hl.bind("ALT + " .. mod .. " + G", hl.dsp.exec_cmd "hypr-focus-or-launch google-chrome google-chrome-stable")
hl.bind("ALT + " .. mod .. " + T", hl.dsp.exec_cmd "hypr-focus-or-launch Alacritty alacritty")
hl.bind(mod .. " + SPACE", hl.dsp.exec_cmd(launcher))

hl.bind(mod .. " + N", hl.dsp.exec_cmd "hypr-new-app-window")
hl.bind(mod .. " + grave", hl.dsp.exec_cmd "hypr-cycle-app-window")
hl.bind(mod .. " + SHIFT + grave", hl.dsp.exec_cmd "hypr-cycle-app-window prev")
hl.bind("ALT + grave", hl.dsp.exec_cmd "hypr-cycle-app-window")
hl.bind("ALT + SHIFT + grave", hl.dsp.exec_cmd "hypr-cycle-app-window prev")

hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd "hyprland-change-volume +", { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd "hyprland-change-volume -", { locked = true, repeating = true })
hl.bind("XF86AudioMute", hl.dsp.exec_cmd "hyprland-change-volume mute", { locked = true })

if hl.plugin and hl.plugin.hyprbars then
  hl.config {
    plugin = {
      hyprbars = {
        bar_height = 26,
        bar_color = "rgb(202020)",
        bar_title_enabled = true,
        bar_text_size = 13,
        bar_text_align = "center",
        bar_padding = 10,
        bar_button_padding = 8,
        bar_buttons_alignment = "left",
        on_double_click = 'hyprctl dispatch \'hl.dsp.window.fullscreen({ mode = "fullscreen", action = "toggle" })\'',
      },
    },
  }

  if type(hl.plugin.hyprbars.add_button) == "function" then
    hl.plugin.hyprbars.add_button {
      bg_color = "rgb(aa4444)",
      fg_color = "rgb(ffffff)",
      size = 14,
      icon = "x",
      action = "hyprctl dispatch 'hl.dsp.window.close()'",
    }
  end
end
