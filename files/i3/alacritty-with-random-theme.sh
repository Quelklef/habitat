#!/usr/bin/env bash

disabled_themes=(
  /home/lark/.config/alacritty/themes/xterm.yaml
  /home/lark/.config/alacritty/themes/low_contrast.yaml
  /home/lark/.config/alacritty/themes/terminal_app.yaml
  /home/lark/.config/alacritty/themes/high_contrast.yaml
  /home/lark/.config/alacritty/themes/tango_dark.yaml
  /home/lark/.config/alacritty/themes/pencil_light.yaml
  /home/lark/.config/alacritty/themes/gruvbox_light.yaml
  /home/lark/.config/alacritty/themes/solarized_light.yaml
  /home/lark/.config/alacritty/themes/papercolor_light.yaml
  /home/lark/.config/alacritty/themes/night_owlish_light.yaml
  /home/lark/.config/alacritty/themes/atom_one_light.yaml
  /home/lark/.config/alacritty/themes/konsole_linux.yaml
)

root=$XDG_CONFIG_HOME/alacritty

read -a all_themes <<< "$(echo $root/themes/*.y{a,}ml)"

function array_diff {
  # https://stackoverflow.com/a/42399479/4608364
  eval local ARR1=\(\"\${$2[@]}\"\)
  eval local ARR2=\(\"\${$3[@]}\"\)
  local IFS=$'\n'
  mapfile -t $1 < <(comm -23 <(echo "${ARR1[*]}" | sort) <(echo "${ARR2[*]}" | sort))
}

array_diff enabled_themes all_themes disabled_themes

choice_idx=$(( $RANDOM % ${#enabled_themes[@]} ))
choice="${enabled_themes[$choice_idx]}"

export ALACRITTY_THEME=$choice

alacritty --config-file <({
  echo "import: [ $choice ]"
  cat $root/alacritty.yml
}) "$@"
