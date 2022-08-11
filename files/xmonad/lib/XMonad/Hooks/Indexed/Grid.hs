{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Wwarn #-}

{- | Two-dimensional workspaces for XMonad -}

module XMonad.Hooks.Indexed.Grid where

import           Prelude

import           Control.Category            ((>>>))
import           Control.Lens                ((%~), (&), (.~), (^.))
import           Data.Foldable               (fold)
import           Data.Generics.Labels        ()
import           Data.List                   (intercalate)
import           Data.List.Split             (splitOn)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 (Endo (..))
import           GHC.Generics                (Generic)
import           XMonad                      hiding (config, state, trace)
import           XMonad.Hooks.StatusBar.PP   (PP (..))
import           XMonad.StackSet             (greedyView, shift)
import qualified XMonad.Util.ExtensibleConf  as XC
import qualified XMonad.Util.ExtensibleState as XS

import qualified XMonad.Hooks.Indexed.Core   as Core
import           XMonad.Hooks.Indexed.Core   (WidMapping (..))

import           Debug.Trace



data Coord = XY { x :: Int, y :: Int }
  deriving (Show, Eq, Ord, Generic)

instance ExtensionClass Coord where
  initialValue = XY 0 0

data Config = Config
  { wids     :: WidMapping Coord
  , wrapping :: Wrapping
  }
  deriving (Generic)

data Wrapping = Wrapping
  { wrapX :: Bool
  , wrapY :: Bool
  }
  deriving (Show, Generic)

instance Semigroup Config where
  _ <> c = c

instance Default Config where
  def = Config mempty (Wrapping False False)

toCore :: Config -> Core.Config Coord
toCore config =
  Core.Config
    { Core.wids = config ^. #wids
    , Core.toName = splitOn ":" >>> drop 1 >>> intercalate ":"
    , Core.toNeighborhood = \(XY _ y) -> (flip XY y) <$> Core.span (^. #x) (config ^. #wids)
    , Core.toLabel = \(XY _ y) -> Just $ show (y + 1) <> " / "
    }

wrap :: Config -> (Coord -> Coord)
wrap config =

  unEndo . fromMaybe (Endo id) $ do
    xRange <- Core.range (^. #x) (config ^. #wids)
    yRange <- Core.range (^. #y) (config ^. #wids)

    pure $ guard (config ^. #wrapping . #wrapX) (Endo $ #x %~ affineMod xRange)
        <> guard (config ^. #wrapping . #wrapY) (Endo $ #y %~ affineMod yRange)

  where

  guard :: Monoid m => Bool -> m -> m
  guard = \case
    True  -> id
    False -> const mempty

  affineMod :: (Ord a, Num a) => (a, a) -> (a -> a)
  affineMod range@(lo, hi) x
    | x > hi = affineMod range (x - (hi - lo + 1))
    | x < lo = affineMod range (x + (hi - lo + 1))
    | otherwise = x

  unEndo :: Endo a -> (a -> a)
  unEndo (Endo f) = f



data Dims = Dims { width :: Int, height :: Int }

-- | Construct a 2d grid
grid :: Dims -> WidMapping Coord
grid = grid' (\(XY x y) -> show y <> "/" <> show x <> ":" <> show (x + 1))

grid' :: (Coord -> WorkspaceId) -> Dims -> WidMapping Coord
grid' toId (Dims { width, height }) =
  WidMapping . fold $ do
    y <- [0 .. height - 1]
    x <- [0 .. width - 1]
    let coord = XY x y
    pure $ Map.singleton coord (toId coord)

-- | Glue together a column of workspaces
column :: Dims -> Int -> WorkspaceId -> WidMapping Coord
column (Dims { height }) x wid =
  WidMapping . fold $ do
    y <- [0 .. height - 1]
    pure $ Map.singleton (XY x y) (":" <> wid)



mapCoord :: (Coord -> Coord) -> X ()
mapCoord f = do
  (coord, config) <- Core.getBoth
  let coord' = coord & f & wrap config
  case Core.getWid (toCore config) coord' of
    Nothing -> pure ()
    Just wid -> do
      XS.put coord'
      windows (greedyView wid)

incX, incY, decX, decY :: X ()
incX = mapCoord $ #x %~ (+1)
incY = mapCoord $ #y %~ (+1)
decX = mapCoord $ #x %~ (+(-1))
decY = mapCoord $ #y %~ (+(-1))

setX :: Int -> X ()
setX = mapCoord . (#x .~)

moveWindowToX :: Int -> X ()
moveWindowToX x = do
  (XY { y }, config) <- Core.getBoth
  case traceShowId $ Core.getWid (toCore config) (XY x y) of
    Nothing  -> pure ()
    Just wid -> windows (shift wid)

hook :: Config -> XConfig l -> XConfig l
hook config = XC.add config >>> Core.modifyXConfig (toCore config)

pp :: X PP
pp = do
  (coord, config) <- Core.getBoth
  pure $ Core.pp (toCore config) coord
