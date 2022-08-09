{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

import XMonad
import XMonad.Layout ((|||))
import qualified XMonad as XMonad
import XMonad.Util.EZConfig (mkKeymap)
import Control.Monad (when)
import Text.Printf (printf)
import System.Posix.Process (executeFile)
import System.Info (arch, os)
import System.Environment (getArgs)
import System.FilePath ((</>))
import qualified XMonad.StackSet as W
import qualified Data.Map as Map
import Data.Map (Map)
import qualified XMonad.Layout.WindowNavigation as LWN
import System.Exit (exitWith, ExitCode (ExitSuccess))
import qualified XMonad.Layout.BinarySpacePartition as LBSP
import Control.Monad.Writer (Writer, execWriter, tell)
import qualified XMonad.Layout.Tabbed as LT
import Data.Foldable (for_)
import Data.Function ((&))
import qualified XMonad.Util.Themes as Themes
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.StatusBar (withSB, statusBarProp)
import XMonad.Hooks.ManageDocks (docks, avoidStruts)


main :: IO ()
main = xmonad . withSB myStatusBar . ewmh . docks $ myConfig


myStatusBar = statusBarProp "xmobar" (pure myPP)
  where myPP = def


myConfig = def
  { modMask = mod4Mask  -- super
  , terminal = "alacritty-random"
  , normalBorderColor = "#000000"
  , focusedBorderColor = "#CC0066"
  , clickJustFocuses = False
  , keys = myKeys
  , layoutHook = LWN.windowNavigation . avoidStruts $ LBSP.emptyBSP ||| myTabbed ||| Full
  }

  where

  myTabbed = LT.tabbed LT.shrinkText def


myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig { XMonad.modMask = mod }) =

  compile conf . execWriter $ do

    -- launch terminal
    bind' "M-<Return>" $ spawn (XMonad.terminal conf)

    -- kill focused window
    bind' "M-q" kill

    -- rotate layout
    bind' "M-<Space>" $ sendMessage NextLayout

    -- move up/down window stack (n for next)
    bind' "M-n"   $ windows W.focusDown
    bind' "M-C-n" $ windows W.focusUp
    bind' "M-S-n"   $ windows W.swapDown
    bind' "M-S-C-n" $ windows W.swapUp

    -- move two-dimensionally
    bind' "M-l"   $ sendMessage (LWN.Go LWN.R)
    bind' "M-j"   $ sendMessage (LWN.Go LWN.D)
    bind' "M-h"   $ sendMessage (LWN.Go LWN.L)
    bind' "M-k"   $ sendMessage (LWN.Go LWN.U)
    bind' "M-S-l" $ sendMessage (LWN.Swap LWN.R)
    bind' "M-S-j" $ sendMessage (LWN.Swap LWN.D)
    bind' "M-S-h" $ sendMessage (LWN.Swap LWN.L)
    bind' "M-S-k" $ sendMessage (LWN.Swap LWN.U)

    -- workspaces
    for_ (zip (XMonad.workspaces conf) [xK_1 .. xK_9]) $ \(i, k) -> do
      for_ [(W.greedyView, 0), (W.shift, shiftMask)] $ \(f, m) -> do
        bind (mod .|. m) k (windows $ f i)

    -- resize
    bind' "M-C-l" $ sendMessage (LBSP.ExpandTowards LBSP.R)
    bind' "M-C-j" $ sendMessage (LBSP.ExpandTowards LBSP.D)
    bind' "M-C-h" $ sendMessage (LBSP.ExpandTowards LBSP.L)
    bind' "M-C-k" $ sendMessage (LBSP.ExpandTowards LBSP.U)
    bind' "M-C-S-l" $ sendMessage (LBSP.ShrinkFrom LBSP.R)
    bind' "M-C-S-j" $ sendMessage (LBSP.ShrinkFrom LBSP.D)
    bind' "M-C-S-h" $ sendMessage (LBSP.ShrinkFrom LBSP.L)
    bind' "M-C-S-k" $ sendMessage (LBSP.ShrinkFrom LBSP.U)

    -- rotate BSP pair
    bind' "M-]" $ sendMessage LBSP.Rotate

    -- screenshot fullscreen
    let scrot = "scrot -q 100 -e 'xclip -selection clipboard -t image/png -i $f; rm $f'"
    -- bind' "M-p f" $ spawn scrot

    -- screenshot selection
    let scrotsel = scrot <> " -s -f -l color=#00ff00"
    bind' "M-p s" $ spawn scrotsel

    -- volume
    bind' "<XF86AudioRaiseVolume>" $ spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%"
    bind' "<XF86AudioLowerVolume>" $ spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%"
    bind' "<XF86AudioMute>"        $ spawn "pactl set-sink-mute   @DEFAULT_SINK@ toggle"
    bind' "<XF86AudioMicMute>"     $ spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle"

    -- brightness
    bind' "<XF86MonBrightnessUp>"   $ spawn "light -A 10"
    bind' "<XF86MonBrightnessDown>" $ spawn "light -U 10"

  where

  shift = shiftMask

  -- old-style binding
  bind :: KeyMask -> KeySym -> X () -> Writer Bindings ()
  bind mask key act = tell [ OldStyle mask key act ]

  -- new-style binding
  bind' :: String -> X () -> Writer Bindings ()
  bind' str act = tell [ NewStyle str act ]


data Binding
  = OldStyle KeyMask KeySym (X ())
  | NewStyle String (X ())

type Bindings = [Binding]

-- prefers new-style bindings in case of conflict!
compile :: XConfig Layout -> Bindings -> Map (KeyMask, KeySym) (X ())
compile config bindings = let

  olds = bindings >>= \case { b@(OldStyle _ _ _) -> [b]; _ -> [] }
  news = bindings >>= \case { b@(NewStyle _ _  ) -> [b]; _ -> [] }

  oldsMap = olds & foldMap (\(OldStyle km ks act) -> Map.singleton (km, ks) act)
  newsMap = news & foldMap (\(NewStyle s act) -> [(s, act)]) & mkKeymap config
  -- nb. mkKeymap does not distribute over list concatenation, which is why
  --     we have to do all the computation here in one place

  in oldsMap <> newsMap
