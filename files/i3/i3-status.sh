#!/usr/bin/env bash

function main {
  echo '{ "version": 1 }'
  echo '['
  trap 'loop & wait' SIGUSR1
  loop & wait
}

function loop {
  while true; do
    status
    echo ','
    sleep 1.2
  done
}

function status {

  local stat_time_a=$(date +'%a %Y-%m-%d')
  local stat_time_b=$(date +'%H:%M:%S')
  local stat_time_c=$(date +'%z')

  local vol_l=$(amixer sget Master | grep 'Left:' | awk -F'[][]' '{ print $2 }')
  local vol_r=$(amixer sget Master | grep 'Right:' | awk -F'[][]' '{ print $2 }')
  local vol_amt=$({ [ "$vol_l" = "$vol_r" ] && echo "$vol_l" || echo "$vol_l/$vol_r"; })
  local stat_volume="vol $vol_amt"

  local bright_n=$(light | xargs printf "%0.f")
  local stat_backlight="bli ${bright_n}%"

  local batt_all=$(acpi -i | grep '0:')
  local batt_degred=$(echo "$batt_all" | tail -n1 | awk -F' = ' '{ print $2 }')
  local batt_percent=$(echo "$batt_all" | head -n1 | grep -oP '\d+(?=%)')

  local batt_charging=$({
    batt_time() { echo "$batt_all" | grep -oP '\d{2}:\d{2}(?=:)'; }
    case "$batt_all" in
      *Charging*) echo -n ' ↑'; batt_time ;;
      *Discharging*) echo -n ' ↓'; batt_time ;;
      *'Not charging'*) echo ' ---:--' ;;
    esac
    
  })

  local stat_battery="bat ${batt_percent}% (×$batt_degred)${batt_charging}"
  local stat_batt_urgent=$( [ "$batt_percent" -le 10 ] && echo true || echo false )

  local cpu_mode=$(cpufreq-info | grep 'The gov' | head -n1 | awk -F\" '{ print $2 }')
  local stat_cpu="cpu $cpu_mode"

  cat << EOF
[
  { "full_text": "${stat_time_a}", "separator": false, "separator_block_width": 8 },
  { "full_text": "${stat_time_b}", "separator": false, "separator_block_width": 8, "color": "#ffaacc" },
  { "full_text": "${stat_time_c}" },
  { "full_text": "${stat_volume}" },
  { "full_text": "${stat_backlight}" },
  { "full_text": "${stat_battery}", "urgent": ${stat_batt_urgent} },
  { "full_text": "${stat_cpu}", "separator": false, "separator_block_width": 8 },
  { "full_text": "" }
]
EOF
}

main
