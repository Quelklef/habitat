{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -fwarn-unused-binds     #-}

{- | Two-dimensional workspaces for XMonad -}

module XMonad.Hooks.Indexed.TwoDimensional where

import           Debug.Trace
import           Prelude                      hiding (span)
import           XMonad.Hooks.Indexed.Core

import           Control.Category             ((>>>))
import           Control.Lens                 (view, (%~), (&), (.~), (^.))
import           Data.Foldable                (fold, toList)
import           Data.Function                (on)
import           Data.Functor                 (($>))
import           Data.Generics.Labels         ()
import           Data.List                    (elemIndex, intercalate, nub)
import           Data.List.Split              (splitOn)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (catMaybes, fromMaybe, isJust)
import           Data.Proxy                   (Proxy (..))
import           GHC.Generics                 (Generic)
import           XMonad                       hiding (config, state)
import           XMonad.Hooks.StatusBar.PP    (PP (..))
import           XMonad.StackSet              (greedyView, shift, tag)
import qualified XMonad.Util.ExtensibleConf   as XC
import qualified XMonad.Util.ExtensibleState  as XS
import           XMonad.Util.WorkspaceCompare (mkWsSort)

data Dims = Dims { width :: Int, height :: Int }

data Coord = XY { x :: Int, y :: Int }
  deriving (Show, Eq, Ord, Generic)

instance ExtensionClass Coord where
  initialValue = XY 0 0

-- | Glue together a column of workspaces
column :: Dims -> Int -> WorkspaceId -> WidMapping Coord
column (Dims { height }) x id =
  WidMapping . fold $ do
    y <- [0 .. height - 1]
    pure $ Map.singleton (XY x y) (":" <> id)

-- | Construct a 2d grid
grid :: Dims -> WidMapping Coord
grid = grid' (\(XY x y) -> show y <> "/" <> show x <> ":" <> show (x + 1))

grid' :: (Coord -> WorkspaceId) -> Dims -> WidMapping Coord
grid' toId (Dims { width, height }) =
  WidMapping . fold $ do
    x <- [0 .. width - 1]
    y <- [0 .. height - 1]
    let coord = XY x y
    pure $ Map.singleton coord (toId coord)

mkConfig :: WidMapping Coord -> Config Coord
mkConfig wids = Config
  { wids = wids
  , toName = splitOn ":" >>> drop 1 >>> intercalate ":"
  , toNeighborhood = \(XY _ y) -> (flip XY y) <$> span (^. #x) wids
  , toLabel = \(XY _ y) -> Just $ show (y + 1) <> " / "
  }

mapCoord :: (Coord -> Coord) -> X ()
mapCoord f = do
  ix <- getIx (Proxy @Coord)
  let coord = ix ^. #coord
  let coord' = f coord
  case getWid (ix ^. #config) coord' of
    Nothing -> pure ()
    Just id -> do
      XS.put coord'
      windows (greedyView id)

incX, incY :: X ()
incX = mapCoord $ #x %~ (+1)
incY = mapCoord $ #y %~ (+1)
decX = mapCoord $ #x %~ (+(-1))
decY = mapCoord $ #y %~ (+(-1))

setX :: Int -> X ()
setX = mapCoord . (#x .~)

moveWindowToX :: Int -> X ()
moveWindowToX x = do
  ix <- getIx (Proxy @Coord)
  let y = ix ^. #coord . #y
  case getWid (ix ^. #config) (XY x y) of
    Nothing  -> pure ()
    Just wid -> windows (shift wid)
