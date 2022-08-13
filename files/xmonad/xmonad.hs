{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}

import           Control.Category                   ((>>>))
import           Control.Lens                       ((%~), (&), (.~), (^.))
import           Control.Monad.Writer               (Writer, execWriter, lift,
                                                     tell)
import           Data.Foldable                      (fold, for_)
import           Data.Function                      (on, (&))
import           Data.List                          (elemIndex)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (catMaybes, fromMaybe)
import           Data.Proxy                         (Proxy (Proxy))
import           XMonad                             hiding (trace)
import           XMonad.Config.Prime                (Query (..))
import           XMonad.Hooks.EwmhDesktops          (ewmh)
import           XMonad.Hooks.ManageDocks           (avoidStruts, docks)
import           XMonad.Hooks.StatusBar             (statusBarProp, withSB)
import           XMonad.Hooks.StatusBar.PP          (PP (..), xmobarColor)
import qualified XMonad.Layout.BinarySpacePartition as LB
import           XMonad.Layout.Decoration           (Theme (..))
import           XMonad.Layout.Reflect              (reflectHoriz, reflectVert)
import           XMonad.Layout.Spacing              (spacingWithEdge)
import qualified XMonad.Layout.Tabbed               as LT
import qualified XMonad.Layout.WindowNavigation     as LW
import           XMonad.StackSet                    (focusDown, focusUp, sink,
                                                     swapDown, swapUp)
import           XMonad.Util.EZConfig               (mkKeymap)
import qualified XMonad.Util.Themes                 as Themes
import           XMonad.Util.Themes                 (theme)
import           XMonad.Util.WorkspaceCompare       (mkWsSort)

import qualified XMonad.Hooks.Indexed.Core          as Ix.Core
import qualified XMonad.Hooks.Indexed.Cycle         as Cycle
import qualified XMonad.Hooks.Indexed.Grid          as Grid


main :: IO ()
main =

  mkConfig
    & docks
    & ewmh
    & withSB myStatusBar
    & Grid.hook gridConfig
    -- & Cycle.hook cycleConfig
    & xmonad

  where

  myStatusBar = statusBarProp "xmobar" mkPP

  cycleConfig :: Cycle.Config
  cycleConfig = Cycle.Config
    { Cycle.width = 7
    , Cycle.workspaces = (:[]) <$> ['a' .. 'z']
    }

  gridConfig :: Grid.Config
  gridConfig =
    let dims = Grid.Dims { Grid.width = 6, Grid.height = 4 }
        mapping = fold
          [ Grid.grid dims
          , Grid.column dims 0 "α"
          , Grid.column dims 3 "γ"
          ]
    in Grid.Config
        { Grid.mapping = Grid.SomeMapping mapping
        , Grid.wrapping = Grid.Wrapping True True
        }

  mkPP :: X PP
  mkPP = do
    pp <- Grid.pp
    pure $ pp
        { ppCurrent = ppCurrent pp >>> pad >>> xmobarColor "white" "#C06"
        , ppHidden = ppHidden pp >>> pad >>> xmobarColor "#BBB" ""
        , ppHiddenNoWindows = ppHiddenNoWindows pp >>> pad >>> xmobarColor "#444" ""
        , ppUrgent = xmobarColor "black" "yellow"
        , ppSep = xmobarColor "#555" "" "  •  "
        , ppOrder = ppOrder pp >>> \(workspaces : _layout : windowTitle : _) -> [workspaces, windowTitle]
        , ppTitle = take 112
        }

  wrap l r s = l <> s <> r
  pad = wrap " " " "


mkConfig :: XConfig _
mkConfig = def
  { modMask = mod4Mask  -- super
  , terminal = "alacritty-random"
  , normalBorderColor = "#000000"
  , focusedBorderColor = "#CC0066"
  , borderWidth = 3
  , clickJustFocuses = False
  , keys = myKeys
  , layoutHook =
      ( (LB.emptyBSP
          & reflectVert & reflectHoriz -- open new windows to down/right instead of up/left
          & spacingWithEdge 2  -- WANT: this seems to somehow be affecting the tabbed layout?
        )
        ||| LT.tabbed LT.shrinkText myTheme
      )
      & avoidStruts
      & LW.windowNavigation
  }

  where

  myTheme = (theme Themes.wfarrTheme)
    { decoHeight = 16
    , fontName = "xft:monospace:size=8"
    }


myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig { terminal, modMask = mod }) =

  compile conf . execWriter $ do

    -- launch terminal
    bind' "M-<Return>" $ spawn terminal

    -- kill focused window
    bind' "M-q" kill

    -- rotate layout
    bind' "M-<Space>" $ sendMessage NextLayout

    -- unfloat focused window
    bind' "M-u" $ withFocused (windows . sink)

    -- cycle throgh windows
    bind' "M-n"   $ windows focusDown
    bind' "M-m"   $ windows focusUp
    bind' "M-S-n" $ windows swapDown
    bind' "M-S-m" $ windows swapUp

    -- move around windows two-dimensionally
    bind' "M-l"   $ sendMessage (LW.Go LW.R)
    bind' "M-j"   $ sendMessage (LW.Go LW.D)
    bind' "M-h"   $ sendMessage (LW.Go LW.L)
    bind' "M-k"   $ sendMessage (LW.Go LW.U)
    bind' "M-S-l" $ sendMessage (LW.Swap LW.R)
    bind' "M-S-j" $ sendMessage (LW.Swap LW.D)
    bind' "M-S-h" $ sendMessage (LW.Swap LW.L)
    bind' "M-S-k" $ sendMessage (LW.Swap LW.U)

--    -- workspaces
--    for_ (zip (XMonad.workspaces conf) [xK_1 .. xK_9]) $ \(workspace, key) -> do
--      for_ [(greedyView, 0), (shift, shiftMask)] $ \(fun, mask) -> do
--        bind (mod .|. mask) key (windows $ fun workspace)

    -- move around workspaces two-dimensionally
    bind' "M-s"   $ Grid.move (#y %~ succ)
    bind' "M-w"   $ Grid.move (#y %~ pred)
    bind' "M-S-s" $ Grid.swap (#y %~ succ)
    bind' "M-S-w" $ Grid.swap (#y %~ pred)
    for_ (zip [0..] [xK_1 .. xK_9]) $ \(x, key) -> do
      bind mod                 key $ Grid.move (#x .~ x)
      bind (mod .|. shiftMask) key $ Grid.swap (#x .~ x)

    bind' "M-<Right>"   $ Cycle.move Cycle.Wrap (#position %~ succ)
    bind' "M-<Left>"    $ Cycle.move Cycle.Wrap (#position %~ pred)
    bind' "M-C-<Right>" $ Cycle.move Cycle.Clamp (#offset %~ succ)
    bind' "M-C-<Left>"  $ Cycle.move Cycle.Clamp (#offset %~ pred)

    -- resize
    bind' "M-C-l"   $ sendMessage (LB.ExpandTowards LB.R)
    bind' "M-C-j"   $ sendMessage (LB.ExpandTowards LB.D)
    bind' "M-C-h"   $ sendMessage (LB.ExpandTowards LB.L)
    bind' "M-C-k"   $ sendMessage (LB.ExpandTowards LB.U)
    bind' "M-C-S-l" $ sendMessage (LB.ShrinkFrom LB.R)
    bind' "M-C-S-j" $ sendMessage (LB.ShrinkFrom LB.D)
    bind' "M-C-S-h" $ sendMessage (LB.ShrinkFrom LB.L)
    bind' "M-C-S-k" $ sendMessage (LB.ShrinkFrom LB.U)

    -- rotate BSP pair
    bind' "M-]" $ sendMessage LB.Rotate

    -- screenshot fullscreen
    let scrot = "scrot -q 100 -e 'xclip -selection clipboard -t image/png -i $f; rm $f'"
    bind' "M-p f" $ spawn scrot

    -- screenshot selection
    let scrotsel = scrot <> " -s -f -l color=#00ff00"
    bind' "M-p s" $ spawn scrotsel

    let andRefreshXmobar = (<> "&& pkill --signal SIGUSR2 xmobar")

    -- volume
    bind' "<XF86AudioRaiseVolume>" $ spawn . andRefreshXmobar $ "pactl set-sink-volume @DEFAULT_SINK@ +10%"
    bind' "<XF86AudioLowerVolume>" $ spawn . andRefreshXmobar $ "pactl set-sink-volume @DEFAULT_SINK@ -10%"
    bind' "<XF86AudioMute>"        $ spawn . andRefreshXmobar $ "pactl set-sink-mute   @DEFAULT_SINK@ toggle"
    bind' "<XF86AudioMicMute>"     $ spawn . andRefreshXmobar $ "pactl set-source-mute @DEFAULT_SOURCE@ toggle"

    -- brightness
    bind' "<XF86MonBrightnessUp>"   $ spawn . andRefreshXmobar $ "light -A 10"
    bind' "<XF86MonBrightnessDown>" $ spawn . andRefreshXmobar $ "light -U 10"

  where

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

