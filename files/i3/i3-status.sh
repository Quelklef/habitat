#!/usr/bin/env bash

function main {
  echo '{ "version": 1 }'
  echo '['
  while true; do
    status
    echo ','
    sleep 1.5
  done
}

function status {

  local stat_time=$(date +'%a %Y-%m-%d %H:%M:%S %z')

  local vol_l=$(amixer sget Master | grep 'Left:' | awk -F'[][]' '{ print $2 }')
  local vol_r=$(amixer sget Master | grep 'Right:' | awk -F'[][]' '{ print $2 }')
  local stat_volume="vol $vol_l/$vol_r"

  local bright_n=$(light | xargs printf "%0.f")
  local stat_backlight="bli ${bright_n}%"

  local batt_all=$(acpi -i | grep '0:')
  local batt_degred=$(echo "$batt_all" | tail -n1 | awk -F' = ' '{ print $2 }')
  local batt_percent=$(echo "$batt_all" | head -n1 | grep -oP '\d+(?=%)')

  local batt_charging=$({
    case "$batt_all" in
      *Charging*) echo -n ' ↑' ;;
      *Discharging*) echo -n ' ↓' ;;
      *'Not charging'*) return 0 ;;
    esac
    echo "$batt_all" | grep -oP '\d{2}:\d{2}(?=:)'
  })

  local stat_battery="bat ${batt_percent}% (×$batt_degred)${batt_charging}"
  local stat_batt_urgent=$( [ "$batt_percent" -le 10 ] && echo true || echo false )

  local cpu_mode=$(cpufreq-info | grep 'The gov' | head -n1 | awk -F\" '{ print $2 }')
  local stat_cpu="cpu $cpu_mode"

  cat << EOF
[
  { "full_text": "${stat_time}" },
  { "full_text": "${stat_volume}" },
  { "full_text": "${stat_backlight}" },
  { "full_text": "${stat_battery}", "urgent": ${stat_batt_urgent} },
  { "full_text": "${stat_cpu}" },
  { "full_text": "" }
]
EOF
}

main
