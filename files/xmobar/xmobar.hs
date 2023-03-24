{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Codec.Binary.UTF8.String (decodeString, encodeString)
import           Control.Concurrent       (threadDelay)
import           Control.Exception        (SomeException, catch)
import           Control.Monad            (forever, void)
import           Data.Char                (toLower)
import           Data.Foldable            (fold)
import           Data.List                (intercalate)
import           System.Posix.Signals     (Handler (..), Signal, SignalInfo,
                                           installHandler, sigUSR2)
import           Text.RawString.QQ        (r)
import           Xmobar
import           XMonad.Util.Run          (runProcessWithInput)

main :: IO ()
main = xmobar config

config :: Config
config = defaultConfig
  { template = fold
      [ " "
      , "%XMonadLog%"
      , "||"
      , let sep = "<fc=#555>  •  </fc>"
        in intercalate sep $
            [ "<action=`perf.switch && pkill --signal SIGUSR2 xmobar`>%cpu%</action>"
            , "%bri%", "%vol%"
            , "%battery-hi%<fc=black,pink>%battery-lo%</fc>"
            , "%uptime%"
            , "%date%"
            ]
      , " "
      ]
  , alignSep = "||"
  , font = "monospace 8"
  , commands =
      [ Run $ Lowercase (Date "%a %Y-%m-%d %H:%M:%S %z" "date" 7)
      , Run XMonadLog
      , Run $ Lowercase (Uptime [] 300)
      , Run battery_hi
      , Run battery_lo
      , Run volume
      , Run brightness
      , Run cpu
      ]
  }

  where

  wrap d s = d <> s <> d

  withPrelude :: String -> String
  withPrelude = ([r|
    function fmt_percent {
      str="$1"
      for (( i = "${#str}"; i < 3; i++ )); do str="·$str"; done
      echo "${str}%"
    }
  |] <>)

  volume = Cmd "vol" 10 $ withPrelude [r|
    function get_vol { amixer sget Master | grep "$1" | grep -Po '[0-9]+(?=%)'; }
    vol_l=$(get_vol Left)
    vol_r=$(get_vol Right)
    echo -n 'vol '
    if [ "$vol_l" = "$vol_r" ]; then
      echo -n "$(fmt_percent $vol_l)"
    else
      echo -n "$(fmt_percent $vol_l)/$(fmt_percent $vol_r)"
    fi
  |]

  brightness = Cmd "bri" 10 $ withPrelude [r|
    bri=$(light | xargs printf "%3d")
    echo -n "bri $(fmt_percent $bri)"
  |]

  battery_hi = Cmd "battery-hi" 5 (batteryScript "[ $batt_percent -gt 10 -o $batt_is_charging -eq 1 ]")
  battery_lo = Cmd "battery-lo" 5 (batteryScript "[ $batt_percent -le 10 -a $batt_is_charging -ne 1 ]")

  batteryScript cond = withPrelude [r|
    batt_all=$(acpi -i | grep '0:')
    batt_degred=$(echo "$batt_all" | tail -n1 | awk -F' = ' '{ print $2 }' | xargs printf '%3d')
    batt_percent=$(echo "$batt_all" | head -n1 | grep -oP '\d+(?=%)' | xargs printf '%3d')

    batt_is_charging=$({
      case "$batt_all" in
        *Charging*) echo -n 1 ;;
        *) echo -n 0 ;;
      esac
    })

    batt_charging=$({
      batt_time() { echo "$batt_all" | grep -oP '\d{2}:\d{2}(?=:)'; }
      case "$batt_all" in
        *Charging*:*'until charged'*) echo -n ' ↑'; batt_time ;;
        *Discharging*) echo -n ' ↓'; batt_time ;;
        *) echo ' ---:--' ;;
      esac
    })

    if ! |] <> cond <> [r|; then
      exit 0
    fi

    echo -n "bat $(fmt_percent $batt_percent) (×$(fmt_percent $batt_degred))${batt_charging}"
  |]

  cpu = Cmd "cpu" 10 [r|
    echo -n "cpu $(perf.which)"
  |]


-- <rant>
-- Why the hell is Exec a typeclass and not a datatype?
-- As far as I can tell, swapping it out for a datatype would greatly simplify the API
-- </rant>


-- |
--
-- Runs a bash command at a given frequrency, producing its stdout
--
-- Upon receiving SIGUSR2, runs the command immediately
data Cmd = Cmd
  { cmdAlias       :: Alias
  , cmdFreqSeconds :: Int
  , cmdCommand     :: String
  }
  deriving (Show, Read)  -- Exec demands it

instance Exec Cmd where
  alias (Cmd al _ _) = al
  start (Cmd _ freq cmd) k = do
    let doit = runBash cmd >>= k
    appendHandler sigUSR2 doit
    everySeconds freq doit

-- Will break if a non-Catch handler exists for the given signal
appendHandler :: Signal -> IO () -> IO ()
appendHandler signal newHandler = do
  Catch oldHandler <- installHandler signal Default Nothing
  void $ installHandler signal (Catch $ oldHandler <> newHandler) Nothing

everySeconds :: Int -> IO () -> IO ()
everySeconds n act = forever (act >> threadDelay (n * 1000000))

runBash :: String -> IO String
runBash cmd = decodeString <$> runProcessWithInput "bash" ["-c", cmd] ""
  -- Can't just use System.Process.readProcess; see https://github.com/xmonad/xmonad/issues/229#issuecomment-660493853
  -- Not quite sure why decodeString is needed for unicode to work, but w/e


-- | Lowercases the output of an Exec instance
newtype Lowercase a = Lowercase a
  deriving (Show, Read)

instance Exec a => Exec (Lowercase a) where
  alias (Lowercase a) = alias a
  start (Lowercase a) cb = start a (cb . map toLower)
