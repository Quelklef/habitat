{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PartialTypeSignatures #-}

import           Control.Category                   ((>>>))
import           Control.Lens                       ((%~), (&), (.~), (^.))
import           Control.Monad.Writer               (Writer, execWriter, tell)
import           Data.Foldable                      (for_)
import           Data.Function                      (on, (&))
import           Data.List                          (elemIndex)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (catMaybes, fromMaybe)
import           XMonad
import           XMonad.Hooks.EwmhDesktops          (ewmh)
import           XMonad.Hooks.ManageDocks           (avoidStruts, docks)
import           XMonad.Hooks.StatusBar             (statusBarGeneric,
                                                     statusBarProp, withSB,
                                                     xmonadPropLog)
import           XMonad.Hooks.StatusBar.PP          (PP (..), dynamicLogString)
import qualified XMonad.Layout.BinarySpacePartition as LBSP
import qualified XMonad.Layout.Tabbed               as LT
import qualified XMonad.Layout.WindowNavigation     as LWN
import qualified XMonad.StackSet                    as W
import           XMonad.Util.EZConfig               (mkKeymap)
import           XMonad.Util.WorkspaceCompare       (mkWsSort)

import qualified W2                                 as W2
import           W2                                 (Coord (XY), W2 (W2), (!?))


main :: IO ()
main =

  mkConfig
    & docks
    & ewmh
    & withSB myStatusBar
    & W2.withW2 w2Config
    & xmonad

  where

  myStatusBar = statusBarGeneric "xmobar" $ do
    w2 <- W2.getW2
    str <- dynamicLogString (mkPP w2)
    let yLabel = W2.getYLabel w2
    xmonadPropLog $ yLabel <> ": "<> str

  mkPP :: W2 -> PP
  mkPP w2 =
    let w2PP = W2.augmentPP w2 def
    in w2PP
        { ppCurrent = ppCurrent w2PP >>> wrap "[" "]"
        , ppHidden = ppHidden w2PP >>> wrap "(" ")"
        , ppHiddenNoWindows = const "..."
        , ppUrgent = wrap "!" "!"
        , ppSep = "  -  "
        , ppTitle = take 150
        }

  wrap l r s = l <> s <> r


mkConfig :: XConfig _
mkConfig = def
  { modMask = mod4Mask  -- super
  , terminal = "alacritty-random"
  , normalBorderColor = "#000000"
  , focusedBorderColor = "#CC0066"
  , clickJustFocuses = False
  , keys = myKeys
  , layoutHook =
      (LBSP.emptyBSP ||| myTabbed ||| Full)
      & avoidStruts
      & LWN.windowNavigation
  }

  where

  myTabbed = LT.tabbed LT.shrinkText def


w2Config :: W2.W2Config
w2Config = W2.W2Config
  { W2.glue = W2.column w2Config 0 "α" <> W2.column w2Config 3 "γ"
  , W2.xLabels = show <$> [1 .. 6]
  , W2.yLabels = show <$> [1 .. 4]
  }


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

--    -- workspaces
--    for_ (zip (XMonad.workspaces conf) [xK_1 .. xK_9]) $ \(workspace, key) -> do
--      for_ [(W.greedyView, 0), (W.shift, shiftMask)] $ \(fun, mask) -> do
--        bind (mod .|. mask) key (windows $ fun workspace)

    -- workspaces
    bind' "M-w"   $ W2.incY
    bind' "M-S-w" $ W2.decY
    for_ (zip [0 .. 10] [xK_1 .. xK_9]) $ \(x, key) -> do
      bind mod                 key (W2.setX x)
      bind (mod .|. shiftMask) key (W2.moveWindowToX x)

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

