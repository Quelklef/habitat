local wezterm = require 'wezterm'


-- Lib --

function trim(str)
  return str:match( "^%s*(.-)%s*$" )
end


-- Config --

local config = wezterm.config_builder()

-- Set color_scheme to the contents of $HOME/.config/wezterm/current-color-scheme
local default_color_scheme = 'Wombat'  -- Used if that file doesn't exist
local color_scheme_file_loc = os.getenv("HOME") .. "/.config/wezterm/current-color-scheme"
wezterm.add_to_config_reload_watch_list(color_scheme_file_loc) -- Reload config when this file changes
local color_scheme_file = io.open(color_scheme_file_loc, "r")
if color_scheme_file ~= nil then
  config.color_scheme = trim(color_scheme_file:read("a"))
  io.close(color_scheme_file)
else
  config.color_scheme = default_color_scheme
end

config.font = wezterm.font 'monospace'
config.font_size = 9

config.hide_tab_bar_if_only_one_tab = true
config.window_padding = { left = 2, right = 2, top = 2, bottom = 2 }
config.window_background_opacity = 0.97

return config
