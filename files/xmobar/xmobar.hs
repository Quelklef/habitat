{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Codec.Binary.UTF8.String (decodeString, encodeString)
import           Control.Exception        (SomeException, catch)
import           Text.RawString.QQ        (r)
import           Xmobar
import           XMonad.Util.Run          (runProcessWithInput)

main :: IO ()
main = xmobar config

config :: Config
config = defaultConfig
  { template = " %XMonadLog% || %cpu%  -  %bri%  -  %vol%  -  %battery%  -  %uptime%  -  %date% "
  , alignSep = "||"
  , font = "DejaVu Sans 6"  -- WANT: this is not working =(
  , commands =
      [ Run $ Date "%a %Y-%m-%d %H:%M:%S %z" "date" 10
      , Run XMonadLog
      , Run $ Uptime [] 10
      , Run battery
      , Run volume
      , Run brightness
      , Run cpu
      ]
  }

  where

  volume = Cmd "vol" [r|
    vol_l=$(amixer sget Master | grep 'Left:' | awk -F'[][]' '{ print $2 }')
    vol_r=$(amixer sget Master | grep 'Right:' | awk -F'[][]' '{ print $2 }')
    vol_amt=$({ [ "$vol_l" = "$vol_r" ] && echo "$vol_l" || echo "$vol_l/$vol_r"; })
    echo -n "Vol $vol_amt"
  |]

  brightness = Cmd "bri" [r|
    bri=$(light | xargs printf "%0.f")
    echo -n "Bri $bri"
  |]

  battery = Cmd "battery" [r|
    batt_all=$(acpi -i | grep '0:')
    batt_degred=$(echo "$batt_all" | tail -n1 | awk -F' = ' '{ print $2 }')
    batt_percent=$(echo "$batt_all" | head -n1 | grep -oP '\d+(?=%)' | xargs printf '%03d')

    batt_charging=$({
      batt_time() { echo "$batt_all" | grep -oP '\d{2}:\d{2}(?=:)'; }
      case "$batt_all" in
        *Charging*) echo -n ' ↑'; batt_time ;;
        *Discharging*) echo -n ' ↓'; batt_time ;;
        *'Not charging'*) echo ' ---:--' ;;
      esac
    })

    echo -n "Batt ${batt_percent}% (×$batt_degred)${batt_charging}"
  |]

  cpu = Cmd "cpu" [r|
    cpu_mode=$(cpufreq-info | grep 'The gov' | head -n1 | awk -F\" '{ print $2 }')
    echo -n "Cpu $cpu_mode"
  |]


-- <rant>
-- Why the hell is Exec a typeclass and not a datatype?
-- As far as I can tell, swapping it out for a datatype would greatly simplify the API
-- </rant>


data Cmd = Cmd Alias String
  deriving (Show, Read, Eq)

instance Exec Cmd where
  alias (Cmd al _) = al
  run (Cmd _ cmd) =

    catch

      (decodeString <$> runProcessWithInput "bash" ["-c", cmd] "")
      -- Can't just use System.Process.readProcess; see https://github.com/xmonad/xmonad/issues/229#issuecomment-660493853
      -- Not quite sure why decodeString is needed for unicodoe to work, but w/e

      (\(e :: SomeException) -> do
        putStrLn $ "err: " <> show e
        pure "<err>"
      )

