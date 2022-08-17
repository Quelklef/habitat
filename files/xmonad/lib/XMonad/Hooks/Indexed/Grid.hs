{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ViewPatterns              #-}
{-# OPTIONS_GHC -Wall -Wwarn #-}

{- | Two-dimensional workspaces for XMonad -}

module XMonad.Hooks.Indexed.Grid where

import           Prelude                       hiding (span)

import           Control.Category              ((>>>))
import           Control.Lens                  ((%~), (&), (.~), (^.))
import           Control.Monad.State           (evalState, execState, modify)
import           Data.Foldable                 (fold, toList)
import           Data.Generics.Labels          ()
import           Data.List                     (intercalate, nub)
import           Data.List.Split               (splitOn)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Maybe                    (catMaybes, fromMaybe)
import           Data.Monoid                   (Endo (..), appEndo)
import           GHC.Generics                  (Generic)
import           XMonad                        hiding (config, state, trace)
import           XMonad.Hooks.StatusBar.PP     (PP (..))
import           XMonad.StackSet               (greedyView, shift)

import qualified XMonad.Hooks.Indexed.Core     as Core
import           XMonad.Hooks.Indexed.Core     (affineMod)

import qualified XMonad.Hooks.Indexed.OneState as OS



data Formatted
  = Unformatted
      -- ^ Unformatted
  | Formatted
      -- ^
      -- Using built-in formatting
      --
      -- This means *specifically* that workspace IDs are of the format
      --  <uniq>:<name>
      -- Where
      -- * <uniq> contains a uniquely-identifying string
      -- * <name> contains the workspace name
      -- * <uniq> may not contain colons
      --
      -- Ideally at compiletime we could pass around a rich object representing such a
      -- format, but I don't think that's possible in Haskell.

doFormat :: forall ftd. DemoteFormatted ftd => Coord -> String -> WorkspaceId
doFormat (XY x y) name =
  case demoteFormatted @ftd of
    Formatted   -> show y <> "/" <> show x <> ":" <> name
    Unformatted -> name

doToName :: forall ftd. DemoteFormatted ftd => WorkspaceId -> String
doToName =
  case demoteFormatted @ftd of
    Unformatted -> id
    Formatted   -> splitOn ":" >>> drop 1 >>> intercalate ":"


class DemoteFormatted (ftd :: Formatted) where
  demoteFormatted :: Formatted

instance DemoteFormatted 'Unformatted where
  demoteFormatted = Unformatted

instance DemoteFormatted 'Formatted where
  demoteFormatted = Formatted


newtype Mapping (ftd :: Formatted) = Mapping { unMapping :: Map Coord WorkspaceId }
  deriving (Show, Generic)

unsafeChangeFormatting :: forall old (ftd :: Formatted). Mapping old -> Mapping ftd
unsafeChangeFormatting (Mapping map) = Mapping map

instance Semigroup (Mapping ftd) where
  Mapping ma <> Mapping mb = Mapping (mb <> ma)  -- right-biased

instance Monoid (Mapping ftd) where
  mempty = Mapping mempty


data SomeMapping = forall ftd. DemoteFormatted ftd => SomeMapping (Mapping ftd)

getTheMap :: SomeMapping -> Map Coord WorkspaceId
getTheMap (SomeMapping (Mapping map)) = map

range :: Ord x => (Coord -> x) -> SomeMapping -> Maybe (x, x)
range proj (getTheMap -> mapping) =
  let xs = proj <$> Map.keys mapping
  in case xs of
      [] -> Nothing
      _  -> Just (minimum xs, maximum xs)

span :: (Ord x, Enum x) => (Coord -> x) -> SomeMapping -> [x]
span proj (getTheMap -> mapping) =
  let xs = proj <$> Map.keys mapping
  in case xs of
      [] -> []
      _  -> [minimum xs .. maximum xs]



data Dims = Dims { width :: Int, height :: Int }

fromMap :: Map Coord WorkspaceId -> Mapping 'Unformatted
fromMap = Mapping

fromFunction :: Dims -> (Coord -> WorkspaceId) -> Mapping 'Unformatted
fromFunction (Dims { width, height }) =
  let domain = XY <$> [0 .. width - 1] <*> [0 .. height - 1]
  in fromMap . funToMap domain

  where
  funToMap :: Ord k => [k] -> (k -> v) -> Map k v
  funToMap xs f = Map.fromList $ (\k -> (k, f k)) <$> xs


-- |
--
-- Construct a 2d grid
--
-- Each coordinate (x, y) will be given the name @show (x + 1)@
grid :: Dims -> Mapping 'Formatted
grid = grid' (\(XY x y) -> show $ x + 1)


-- |
--
-- Construct a 2d grid
--
-- Each coordinate will be assigned a name according to the supplied function
grid' :: (Coord -> String) -> Dims -> Mapping 'Formatted
grid' toName (Dims { width, height }) =
  Mapping . fold $ do
    y <- [0 .. height - 1]
    x <- [0 .. width - 1]
    let coord = XY x y
    pure $ Map.singleton coord (doFormat @'Formatted coord $ toName coord)


-- | Glue together a group of workspaces
group :: forall f ftd. (DemoteFormatted ftd, Foldable f) => f Coord -> String -> Mapping ftd
group (toList -> xs) name = case xs of
  []     -> Mapping mempty
  (x0:_) -> Mapping $ xs & foldMap (\x -> Map.singleton x (doFormat @ftd x0 name))


-- | Glue together a column of workspaces
column :: forall ftd. DemoteFormatted ftd => Dims -> Int -> String -> Mapping ftd
column (Dims { height }) x name =
  Mapping . fold $ do
    y <- [0 .. height - 1]
    let topLeft = XY x 0
    pure $ Map.singleton (XY x y) (doFormat @ftd topLeft name)




data Coord = XY { x :: Int, y :: Int }
  deriving (Show, Ord, Eq, Generic)

data Wrapping = Wrapping
  { wrapX :: Bool
  , wrapY :: Bool
  }
  deriving (Show, Generic)

data State = State
  { mapping  :: SomeMapping
  , wrapping :: Wrapping
  , coord    :: Coord
  } deriving (Generic)

instance OS.OneState State where
  type Mod State = State -> State
  merge ma s = pure (ma s)
  defaultState = State
    { mapping = SomeMapping (grid $ Dims 5 5)
    , wrapping = Wrapping False False
    , coord = XY 0 0
    }

-- Wrap a coordinate around the x/y axes according to the configured wrapping mode
wrap :: State -> (Coord -> Coord)
wrap state =

  appEndo . fromMaybe (Endo id) $ do
    xRange <- range (^. #x) (state ^. #mapping)
    yRange <- range (^. #y) (state ^. #mapping)

    pure $ guard (state ^. #wrapping . #wrapX) (Endo $ #x %~ affineMod xRange)
        <> guard (state ^. #wrapping . #wrapY) (Endo $ #y %~ affineMod yRange)

  where

  guard :: Monoid m => Bool -> m -> m
  guard = \case { True -> id; False -> const mempty }



-- |
--
-- The call @move f@ replaces the current coordinate to @f currentCoord@
--
-- If @f currentCoord@ is out-of-bounds, do nothing
--
-- Use this to move around workspaces
move :: (Coord -> Coord) -> X ()
move = update $ \coord wid -> do
  OS.modify (#coord .~ coord :: State -> State)
  windows (greedyView wid)


-- |
--
-- The call @swap f@ moves the selected window to @f currentCoord@
--
-- If @f currentCoord@ is out-of-bounds, do nothing
swap :: (Coord -> Coord) -> X ()
swap = update $ \_ wid -> windows (shift wid)

update :: (Coord -> WorkspaceId -> X ()) -> (Coord -> Coord) -> X ()
update act f = do
  state@State { coord, mapping } <- OS.get
  let coord' = coord & f & wrap state
  case Map.lookup coord' (getTheMap mapping) of
    Nothing -> pure ()
    Just wid -> do
     act coord' wid


data Init = Init
  { initMapping  :: SomeMapping
  , initWrapping :: Wrapping
  }

-- | Hook the grid layout into XMonad
hook :: Init -> XConfig l -> XConfig l
hook Init { initMapping, initWrapping } =
  OS.once @State
    (\xc -> xc { XMonad.workspaces = workspaces })
    (\state -> state { mapping = initMapping, wrapping = initWrapping })

  where
  workspaces = toList (getTheMap initMapping)
             & nub  -- account for two coordinates pointing to the same workspace

pp :: X PP
pp = do
  State { coord = XY x y, mapping } <- OS.get
  pure $ def
       & Core.withLabel (show (y + 1) <> " / ")
       & Core.withNeighborhood
          (let coords = (flip XY y) <$> span (^. #x) mapping
           in coords & fmap (flip Map.lookup (getTheMap mapping)) & catMaybes & nub)
       & Core.withNameTransform (case mapping of SomeMapping (_ :: Mapping ftd) -> doToName @ftd)

