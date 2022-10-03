{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

import           Control.Category                   ((>>>))
import           Control.Lens                       ((%~), (&), (.~), (^.))
import           Control.Monad                      (void, when)
import           Control.Monad.Writer               (Writer, execWriter, lift,
                                                     tell)
import           Data.Foldable                      (fold, for_)
import           Data.Function                      (on, (&))
import           Data.IORef                         (modifyIORef, newIORef,
                                                     readIORef)
import           Data.List                          (elemIndex)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (catMaybes, fromMaybe)
import           Data.Proxy                         (Proxy (Proxy))
import           System.IO.Unsafe                   (unsafePerformIO)
import           XMonad                             hiding (trace)
import           XMonad.Config.Prime                (Query (..))
import           XMonad.Hooks.EwmhDesktops          (ewmh)
import           XMonad.Hooks.ManageDocks           (avoidStruts, docks)
import           XMonad.Hooks.StatusBar             (statusBarProp, withSB)
import           XMonad.Hooks.StatusBar.PP          (PP (..), xmobarColor)
import qualified XMonad.Layout.BinarySpacePartition as LB
import           XMonad.Layout.Decoration           (Theme (..))
import qualified XMonad.Layout.Gaps                 as Gaps
import           XMonad.Layout.Reflect              (reflectHoriz, reflectVert)
import           XMonad.Layout.Spacing              (spacingWithEdge)
import qualified XMonad.Layout.Tabbed               as LT
import qualified XMonad.Layout.WindowNavigation     as LW
import           XMonad.StackSet                    (focusDown, focusUp, sink,
                                                     swapDown, swapUp)
import           XMonad.Util.EZConfig               (mkKeymap)
import qualified XMonad.Util.Hacks                  as Hacks
import qualified XMonad.Util.Themes                 as Themes
import           XMonad.Util.Themes                 (theme)
import           XMonad.Util.WorkspaceCompare       (mkWsSort)

import qualified XMonad.WorkspaceLayout.Core        as WSL.Core
import qualified XMonad.WorkspaceLayout.Cycle       as Cycle
import qualified XMonad.WorkspaceLayout.Grid        as Grid


data WSLChoice = WSLGrid | WSLCycle
  deriving (Show, Eq)

wslChoice :: WSLChoice
wslChoice = WSLGrid


main :: IO ()
main =

  mkConfig
    & docks
    & ewmh
    & withSB myStatusBar
    & (case wslChoice of
        WSLGrid  -> Grid.hook gridInit
        WSLCycle -> Cycle.hook cycleConfig)
    & xmonad

  where

  myStatusBar = statusBarProp "xmobar" mkPP

  cycleConfig :: Cycle.Config
  cycleConfig = Cycle.Config
    { Cycle.width = 7
    , Cycle.workspaces = (:[]) <$> ['a' .. 'z']
    }

  gridInit :: Grid.Init
  gridInit =
    let dims = Grid.Dims { Grid.width = 9, Grid.height = 4 }
        mapping = fold
          [ dims & Grid.grid' (\coord -> show (Grid.x coord))
          , Grid.column dims 0 "α"
          , Grid.column dims 5 "γ"
          ]
    in Grid.Init
        { Grid.initMapping = Grid.SomeMapping mapping
        , Grid.initWrapping = Grid.Wrapping True True
        , Grid.initLabelf = const Nothing
        }

  mkPP :: X PP
  mkPP = do
    pp <- WSL.Core.render <$>
            (case wslChoice of
              WSLGrid  -> Grid.getView
              WSLCycle -> Cycle.getView)
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
  -- , terminal = "alacritty-random"
        -- WANT^ alacritty random themes are not working for some reason
  , terminal = "alacritty"
  , normalBorderColor = "#000000"
  , focusedBorderColor = "#CC0066"
  , borderWidth = 3
  , clickJustFocuses = False
  , keys = myKeys
  , handleEventHook = handleEventHook def <> Hacks.windowedFullscreenFixEventHook
      -- ^ Fixes an issue with fullscreen on some apps, like chromium
  , layoutHook =
      (
        (
          (
            LB.emptyBSP
            & reflectVert & reflectHoriz -- open new windows to down/right instead of up/left
            & spacingWithEdge 2  -- WANT: this seems to somehow be affecting the tabbed layout?
          )
          ||| LT.tabbed LT.shrinkText myTheme
        )
        & avoidStruts
      )
      ||| Full
      & LW.windowNavigation
      & Gaps.gaps ((, 0) <$> [Gaps.U, Gaps.R, Gaps.D, Gaps.L])
          -- ^ Start with all gaps enabled but empty
          --   This is different from having all gaps disabled!
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

    initializeSlouchmodes

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

    -- resize windows
    -- controls are flipped due to use of reflect(Horiz|Vert) on layout
    bind' "M-C-l"   $ sendMessage (LB.ExpandTowards LB.L)
    bind' "M-C-j"   $ sendMessage (LB.ExpandTowards LB.U)
    bind' "M-C-h"   $ sendMessage (LB.ExpandTowards LB.R)
    bind' "M-C-k"   $ sendMessage (LB.ExpandTowards LB.D)
    bind' "M-C-S-l" $ sendMessage (LB.ShrinkFrom LB.R)
    bind' "M-C-S-j" $ sendMessage (LB.ShrinkFrom LB.D)
    bind' "M-C-S-h" $ sendMessage (LB.ShrinkFrom LB.L)
    bind' "M-C-S-k" $ sendMessage (LB.ShrinkFrom LB.U)

    -- rotate BSP pair
    bind' "M-]" $ sendMessage LB.Rotate

    -- move around workspaces two-dimensionally
    when (wslChoice == WSLGrid) $ do
      bind' "M-s"   $ Grid.move (\c -> c { Grid.y = succ (Grid.y c) })
      bind' "M-w"   $ Grid.move (\c -> c { Grid.y = pred (Grid.y c) })
      bind' "M-S-s" $ Grid.swap (\c -> c { Grid.y = succ (Grid.y c) })
      bind' "M-S-w" $ Grid.swap (\c -> c { Grid.y = pred (Grid.y c) })
      let pairs =
            (<>) [ (0, xK_quoteleft) ]
                 (zip [1..] [xK_1 .. xK_9])
      for_ pairs $ \(x, key) -> do
        bind mod                 key $ Grid.move (\c -> c { Grid.x = x })
        bind (mod .|. shiftMask) key $ Grid.swap (\c -> c { Grid.x = x })

    when (wslChoice == WSLCycle) $ do
      bind' "M-<Right>"   $ Cycle.move Cycle.Wrap (\c -> c { Cycle.position = succ (Cycle.position c) })
      bind' "M-<Left>"    $ Cycle.move Cycle.Wrap (\c -> c { Cycle.position = pred (Cycle.position c) })
      bind' "M-C-<Right>" $ Cycle.move Cycle.Clamp (\c -> c { Cycle.offset = succ (Cycle.offset c) })
      bind' "M-C-<Left>"  $ Cycle.move Cycle.Clamp (\c -> c { Cycle.offset = pred (Cycle.offset c) })

    -- screenshot fullscreen
    let scrot = "scrot -q 100 -e 'xclip -selection clipboard -t image/png -i $f; rm $f'"
    bind' "M-p f" $ spawn scrot

    -- screenshot selection
    let scrotsel = scrot <> " -s -f -l color=#00ff00"
    bind' "M-p s" $ spawn scrotsel

    -- kdfpass
    bind' "M-o" $ spawn "kdfpass"

    let andRefreshXmobar = (<> "&& pkill --signal SIGUSR2 xmobar")

    -- volume
    bind' "<XF86AudioRaiseVolume>" $ spawn . andRefreshXmobar $ "pactl set-sink-volume @DEFAULT_SINK@ +5%"
    bind' "<XF86AudioLowerVolume>" $ spawn . andRefreshXmobar $ "pactl set-sink-volume @DEFAULT_SINK@ -5%"
    bind' "<XF86AudioMute>"        $ spawn . andRefreshXmobar $ "pactl set-sink-mute   @DEFAULT_SINK@ toggle"
    bind' "<XF86AudioMicMute>"     $ spawn . andRefreshXmobar $ "pactl set-source-mute @DEFAULT_SOURCE@ toggle"

    -- brightness
    bind' "<XF86MonBrightnessUp>"   $ spawn . andRefreshXmobar $ "light -A 10"
    bind' "<XF86MonBrightnessDown>" $ spawn . andRefreshXmobar $ "light -U 10"

  where

  -- When I slouch, my fingers obscure the bottom of my screen
  -- Solution: raise all windows above my fingers
  -- Keybinding is p for 'posture', as in 'bad posture'
  initializeSlouchmodes :: Writer Bindings ()
  initializeSlouchmodes = let

      slouchmodes :: [Gaps.GapSpec]
      slouchmodes = [ [(Gaps.D, 0)], [(Gaps.D, 110)], [(Gaps.D, 220)] ]

      next :: Eq a => [a] -> (a -> a)
      next as a = case a `elemIndex` as of
        Nothing  -> as !! 0
        Just idx -> as !! (succ idx `Prelude.mod` length as)

      -- Used to synchronize slouchmode between all layouts
      ref = unsafePerformIO (newIORef $ slouchmodes !! 0)

      in bind' "M-S-p" $ do
        -- Rotate slouch modes
        io $ modifyIORef ref (next slouchmodes)
        -- Set current mode in all windows
        broadcastMessage =<< (Gaps.setGaps <$> io (readIORef ref))
        -- Redraw with new mode
        refresh


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


