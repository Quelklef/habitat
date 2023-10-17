{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Codec.Binary.UTF8.String (decodeString, encodeString)
import           Control.Concurrent       (threadDelay)
import           Control.Exception        (SomeException (..), catch, handle)
import           Control.Monad            (forever, void)
import           Data.Char                (toLower)
import           Data.Foldable            (fold)
import           Data.Function            ((&))
import           Data.List                (intercalate)
import           System.Exit              (ExitCode (..))
import           System.IO                (hClose, hGetLine)
import           System.IO.Error          (isEOFError)
import           System.Posix.Signals     (Handler (..), Signal, SignalInfo,
                                           installHandler, sigUSR2)
import           System.Process           (runInteractiveProcess,
                                           waitForProcess)
import           Text.RawString.QQ        (r)
import           Xmobar

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
            [ "%timer%"
            , "<action=`perf.switch && pkill --signal SIGUSR2 xmobar`>%cpu%</action>"
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
      , Run timerStatus
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

  -- Show a colored bar whenever I'm on the clock
  timerStatus = Cmd "timer" 7 [r|
    timer_info=$(cat /per/state/tt/tt.json | jq .timer)
    if [ "$timer_info" != null ]; then
      timer_start=$( echo "$timer_info" | jq -r .start )
      timer_duration=$( echo "$timer_info" | jq -r .start | node -e '
        const d = new Date(require("fs").readFileSync(0));
        const dur_ms = (+new Date) - (+d);
        const dur_sec = Math.round(dur_ms / 1000);
        const ss = dur_sec % 60;
        const mm = Math.floor(dur_sec / 60) % 60;
        const hh = Math.floor(dur_sec / (60 * 60));
        const to_s = xx => ("" + xx).padStart(2, "0");
        console.log(`${to_s(hh)}:${to_s(mm)}:${to_s(ss)}`);
      ' )
      timer_bucket=$( echo "$timer_info" | jq -r .bucket )
      col=darkgreen
      pad="<fc=$col,$col>"; for (( i = 0; i < 4; i++ )); do pad="${pad}—"; done; pad="${pad}</fc>"
      echo -n "${pad}<fc=white,$col>🕑 ${timer_bucket} ${timer_duration}</fc>${pad}"
    else
      echo -n '×'
    fi
  |]


-- <rant>Why the hell is Exec a typeclass and not a datatype?</rant>


-- |
--
-- Runs a bash command at a given frequency, producing its stdout
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
runBash cmd = do

  {-

  Implementation based on [1]

  Be very careful making changes to this function! So far it's undergone
  at least three different iterations to deal with various issues; namely:

  - It straight up not working
  - Unicode not being properly handled
  - Zombie processes being left behind

  [1]: <https://codeberg.org/xmobar/xmobar/src/commit/b5e397b1fdb9867b1d4ac599c88ef8b6354b5782/src/Xmobar/Plugins/Command.hs>

  -}

  (hStdin, hStdout, hStderr, pid) <- runInteractiveProcess "/usr/bin/env" ["bash", "-c", cmd] Nothing Nothing
  hClose hStdin
  exitCode <- waitForProcess pid
  case exitCode of
    ExitSuccess -> do
      result <- hGetLine hStdout
        & handle (\(err :: IOError) -> pure $ if isEOFError err then "" else "<err: hGetLine failed with an IOError other than an eof error>")
        & handle (\(_ :: SomeException) -> pure "<err: hGetLine failed with some exception (not an IOError)>")
      hClose hStdout >> hClose hStderr
      pure result
    _ -> do
      hClose hStdout >> hClose hStderr
      pure "<err: nonzero exit code>"


-- | Lowercases the output of an Exec instance
newtype Lowercase a = Lowercase a
  deriving (Show, Read)

instance Exec a => Exec (Lowercase a) where
  alias (Lowercase a) = alias a
  start (Lowercase a) cb = start a (cb . map toLower)
