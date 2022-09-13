#!/usr/bin/env bash

root=$XDG_CONFIG_HOME/alacritty

disabled_themes=(
  $root/themes/xterm.yaml
  $root/themes/low_contrast.yaml
  $root/themes/terminal_app.yaml
  $root/themes/high_contrast.yaml
  $root/themes/tango_dark.yaml
  $root/themes/pencil_light.yaml
  $root/themes/gruvbox_light.yaml
  $root/themes/solarized_light.yaml
  $root/themes/papercolor_light.yaml
  $root/themes/night_owlish_light.yaml
  $root/themes/atom_one_light.yaml
  $root/themes/konsole_linux.yaml
)

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
