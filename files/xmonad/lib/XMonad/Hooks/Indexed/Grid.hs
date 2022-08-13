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
{-# LANGUAGE ViewPatterns              #-}
{-# OPTIONS_GHC -Wall -Wwarn #-}

{- | Two-dimensional workspaces for XMonad -}

module XMonad.Hooks.Indexed.Grid where

import           Prelude                     hiding (span)

import           Control.Category            ((>>>))
import           Control.Lens                ((%~), (&), (.~), (^.))
import           Control.Monad.State         (State, evalState, execState,
                                              modify)
import           Data.Foldable               (fold, toList)
import           Data.Generics.Labels        ()
import           Data.List                   (intercalate, nub)
import           Data.List.Split             (splitOn)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe                  (catMaybes, fromMaybe)
import           Data.Monoid                 (Endo (..), appEndo)
import           GHC.Generics                (Generic)
import           XMonad                      hiding (config, state, trace)
import           XMonad.Hooks.StatusBar.PP   (PP (..))
import           XMonad.StackSet             (greedyView, shift)
import qualified XMonad.Util.ExtensibleConf  as XC
import qualified XMonad.Util.ExtensibleState as XS

import qualified XMonad.Hooks.Indexed.Core   as Core
import           XMonad.Hooks.Indexed.Core   (affineMod)



data Formatted
  = Unformatted
      -- ^ Unformatted
  | Formatted
      -- ^
      --
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
  deriving (Show, Eq, Generic)

unsafeChangeFormatting :: forall old (ftd :: Formatted). Mapping old -> Mapping ftd
unsafeChangeFormatting (Mapping map) = Mapping map

instance Semigroup (Mapping ftd) where
  Mapping ma <> Mapping mb = Mapping (mb <> ma)  -- right-biased

instance Monoid (Mapping ftd) where
  mempty = Mapping mempty


data SomeMapping = forall ftd. DemoteFormatted ftd => SomeMapping { unSomeMapping :: Mapping ftd }

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

-- | Construct a 2d grid
grid :: Dims -> Mapping 'Formatted
grid (Dims { width, height }) =
  Mapping . fold $ do
    y <- [0 .. height - 1]
    x <- [0 .. width - 1]
    let coord = XY x y
    pure $ Map.singleton coord (doFormat @'Formatted coord (show $ x + 1))

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
  deriving (Show, Eq, Ord, Generic)

instance ExtensionClass Coord where
  initialValue = XY 0 0

data Config = Config
  { mapping  :: SomeMapping
  , wrapping :: Wrapping
  } deriving (Generic)

data Wrapping = Wrapping
  { wrapX :: Bool
  , wrapY :: Bool
  }
  deriving (Show, Generic)

instance Semigroup Config where
  _ <> c = c

instance Default Config where
  def = Config (SomeMapping (mempty :: Mapping 'Formatted)) (Wrapping False False)

-- Wrap a coordinate around the x/y axes according to the configured wrapping mode
wrap :: Config -> (Coord -> Coord)
wrap config =

  appEndo . fromMaybe (Endo id) $ do
    xRange <- range (^. #x) (config ^. #mapping)
    yRange <- range (^. #y) (config ^. #mapping)

    pure $ guard (config ^. #wrapping . #wrapX) (Endo $ #x %~ affineMod xRange)
        <> guard (config ^. #wrapping . #wrapY) (Endo $ #y %~ affineMod yRange)

  where

  guard :: Monoid m => Bool -> m -> m
  guard = \case { True -> id; False -> const mempty }



-- |
--
--
-- The call @move f@ replaces the current coordinate to @f currentCoord@
--
-- If @f currentCoord@ is out-of-bounds, do nothing
--
-- Use this to move around workspaces
move :: (Coord -> Coord) -> X ()
move = update $ \coord wid -> do
  XS.put coord
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
  (coord, config) <- Core.getBoth
  let coord' = coord & f & wrap config
  case Map.lookup coord' (getTheMap $ config ^. #mapping) of
    Nothing -> pure ()
    Just wid -> do
     act coord' wid


hook :: Config -> XConfig l -> XConfig l
hook config@Config { mapping } = XC.add config >>> (\xc -> xc { XMonad.workspaces = workspaces })
  where
  workspaces = toList (getTheMap mapping)
             & nub  -- account for two coordinates pointing to the same workspace

pp :: X PP
pp = do
  (XY x y :: Coord, Config { mapping }) <- Core.getBoth
  pure $ def
       & Core.withLabel (show (y + 1) <> " / ")
       & Core.withNeighborhood
          (let coords = (flip XY y) <$> span (^. #x) mapping
           in coords & fmap (flip Map.lookup (getTheMap mapping)) & catMaybes & nub)
       & Core.withNameTransform (case mapping of SomeMapping (_ :: Mapping ftd) -> doToName @ftd)

