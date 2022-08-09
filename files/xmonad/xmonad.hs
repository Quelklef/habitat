{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}


import           Control.Category                   ((>>>))
import           Control.Lens                       ((%~), (&), (.~), (^.))
import           Control.Monad                      (when, (>=>))
import           Control.Monad.Writer               (Writer, execWriter, tell)
import           Data.Foldable                      (for_)
import           Data.Function                      (on, (&))
import           Data.Generics.Labels               ()
import           Data.List                          (elemIndex, nub)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (catMaybes)
import           Data.Void                          (Void)
import           GHC.Generics                       (Generic)
import           System.Environment                 (getArgs)
import           System.Exit                        (ExitCode (ExitSuccess),
                                                     exitWith)
import           System.FilePath                    ((</>))
import           System.Info                        (arch, os)
import           System.Posix.Process               (executeFile)
import           Text.Printf                        (printf)
import qualified XMonad                             as XMonad
import           XMonad
import           XMonad.Config.Prime                (ExtensionClass, Message)
import           XMonad.Hooks.EwmhDesktops          (ewmh)
import           XMonad.Hooks.ManageDocks           (avoidStruts, docks)
import           XMonad.Hooks.StatusBar             (statusBarGeneric,
                                                     statusBarProp, withSB,
                                                     xmonadPropLog)
import           XMonad.Hooks.StatusBar.PP          (PP (..), dynamicLogString)
import           XMonad.Layout                      ((|||))
import qualified XMonad.Layout.BinarySpacePartition as LBSP
import           XMonad.Layout.LayoutModifier       (LayoutModifier (handleMess),
                                                     ModifiedLayout (..))
import qualified XMonad.Layout.Tabbed               as LT
import qualified XMonad.Layout.WindowNavigation     as LWN
import qualified XMonad.StackSet                    as W
import qualified XMonad.Util.ExtensibleState        as XS
import           XMonad.Util.EZConfig               (mkKeymap)
import qualified XMonad.Util.Themes                 as Themes
import           XMonad.Util.WorkspaceCompare       (mkWsSort)


main :: IO ()
main =

  mkConfig
    & docks
    & ewmh
    & withSB myStatusBar
    & xmonad

  where

  myStatusBar = statusBarGeneric "xmobar" $ do
    w2 :: W2 Window <- XS.get
    str <- dynamicLogString (mkPP w2)
    let prefix =
          case (w2 ^. #w2config . #yAxis) !? (w2 ^. #w2state . #loc . #y) of
            Nothing    -> "?!"
            Just yName -> yName <> ": "
    xmonadPropLog $ prefix <> str

  mkPP :: W2 Window -> PP
  mkPP w2 = let

    XY x y = w2 ^. #w2state . #loc

    row :: [String]
    row = catMaybes $ toWsName (w2config w2) . flip XY y <$> toIndices (w2 ^. #w2config . #xAxis)

    -- WANT: this is pretty hacky :~/
    unformat :: String -> String
    unformat = reverse >>> takeWhile (/= ':') >>> reverse

    in def
      { ppCurrent = unformat >>> wrap "[" "]"
      , ppHidden = unformat
      , ppSort = do
          sort <- (mkWsSort . pure) (compare `on` flip elemIndex row)
          pure $ filter (W.tag >>> (`elem` row))
                 >>> sort
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
      & w2 w2Config
  , workspaces = w2GetWorkspaces w2Config
  }

  where

  myTabbed = LT.tabbed LT.shrinkText def

  w2Config :: W2Config
  w2Config = W2Config
    { glue = column w2Config 0 "α" <> column w2Config 3 "γ"
    , xAxis = show <$> [1 .. 6]
    , yAxis = show <$> [1 .. 4]
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
    bind' "M-w"   $ sendMessage incY
    bind' "M-S-w" $ sendMessage decY
    for_ (zip [0 .. 10] [xK_1 .. xK_9]) $ \(x, key) -> do
      bind mod                 key (sendMessage $ setX x)
      bind (mod .|. shiftMask) key (sendMessage $ moveWindowToX x)

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




-------------------------------------------------------------------------------
-- Two-dimensional workspaces                                                --
-------------------------------------------------------------------------------


-- Allows a workspace to appear at multiple coordinates
--
-- To have two coordinates p1 and p2 share a workspace, assign
-- them the same value in in the Glue map. (Except Nothing)
--
-- nb. I much prefer the more definition
--   data Glue a = forall rep. Eq rep => Glue (a -> rep)
-- which allows general construction of equivalence relations
-- But we can't use that because we need stuff like
-- a Show Glue instance =(
newtype Glue = Glue (Map Coord String)
  deriving (Show, Read, Eq, Ord)
  deriving newtype (Monoid, Semigroup)

-- Glue a column together
column :: W2Config -> Int -> String -> Glue
column (W2Config { yAxis }) x name =
  toIndices yAxis & foldMap (\y -> Glue $ Map.singleton (XY x y) name)


data W2 a = W2
  { w2config :: W2Config
  , w2state  :: W2State
  }
  deriving (Show, Read)  -- xmonad demands it
  deriving (Typeable)
  deriving (Generic)

-- Allows (W2 Window) to be read from and written into the X monad
instance ExtensionClass (W2 Window) where
  initialValue = undefined  -- tee-hee

data Coord = XY { x :: Int, y :: Int }
  deriving (Show, Read, Eq, Ord, Generic)

data W2Config = W2Config
  { glue  :: Glue
  , xAxis :: [String]
  , yAxis :: [String]
  } deriving (Show, Read, Eq, Ord, Generic)

w2GetWorkspaces :: W2Config -> [String]
w2GetWorkspaces config@(W2Config { xAxis, yAxis }) =
  (do
    y <- toIndices yAxis
    x <- toIndices xAxis
    maybeToList . toWsName config $ XY x y)
  & nub  -- account for duplicate names due to gluing

  where
  maybeToList = \case { Nothing -> []; Just a -> [a]; }

toIndices :: [a] -> [Int]
toIndices xs = take (length xs) [0..]

toWsName :: W2Config -> Coord -> Maybe String
toWsName (W2Config { xAxis, yAxis, glue = Glue glue }) (XY x y) =

  case (xAxis !? x, yAxis !? y) of

    (Just x', Just y') ->
      -- in-bounds
      case Map.lookup (XY x y) glue of
        Just name -> Just name
        Nothing   -> Just $ y' <> ":" <> x'

    _ ->
      -- out-of-bounds
      Nothing

(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n >= 0 && n < length xs = Just (xs !! n)
  | otherwise = Nothing

data W2State = W2State
  { loc :: Coord
  } deriving (Show, Read, Eq, Ord, Generic)

initialState :: W2State
initialState = W2State
  { loc = XY 0 0
  }

instance LayoutModifier W2 Window where
  handleMess w2 msg =
    case fromMessage msg of
      Nothing         -> pure Nothing
      Just (WithW2 f) ->
          f w2 >>= \case
            Nothing -> pure Nothing
            Just w2' -> do
              XS.put w2'  -- record state
              pure (Just w2')

data W2Message = WithW2 (forall a. W2 a -> X (Maybe (W2 a)))
  deriving (Typeable)

instance Message W2Message

w2exec :: (forall a. W2 a -> X ()) -> W2Message
w2exec f = WithW2 (\w2 -> Nothing <$ f w2)

mapCoord :: (Coord -> Coord) -> W2Message
mapCoord f = WithW2 $ \w2 -> do
  let w2' = w2 & #w2state . #loc %~ f
  let wsName = toWsName (w2' ^. #w2config) (w2' ^. #w2state . #loc)
  case wsName of
    Nothing -> pure Nothing  -- moved out-of-bounds
    Just ws -> do
      windows (W.greedyView ws)
      pure (Just w2')

incX, decX, incY, decY :: W2Message
incX = mapCoord $ #x %~ (+1)
decX = mapCoord $ #x %~ (+(-1))
incY = mapCoord $ #y %~ (+1)
decY = mapCoord $ #y %~ (+(-1))

setX, setY :: Int -> W2Message
setX x = mapCoord $ #x .~ x
setY y = mapCoord $ #y .~ y

moveWindowToX :: Int -> W2Message
moveWindowToX x = w2exec $ \w2 -> do
  let y = w2 ^. #w2state . #loc . #y
  case toWsName (w2config w2) (XY x y) of
    Nothing   -> pure ()
    Just name -> windows (W.shift name)

w2 :: W2Config -> l a -> ModifiedLayout W2 l a
w2 config layout = ModifiedLayout (W2 config initialState) layout
