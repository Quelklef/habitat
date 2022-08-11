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

module XMonad.Hooks.Indexed.Core where

import           Prelude                      hiding (span)

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

import           Debug.Trace

newtype WidMapping coord = WidMapping { unWidMapping :: Map coord WorkspaceId }
  deriving (Show, Eq, Generic)

instance Ord coord => Semigroup (WidMapping coord) where
  WidMapping ma <> WidMapping mb = WidMapping (mb <> ma)  -- right-biased

instance Ord coord => Monoid (WidMapping coord) where
  mempty = WidMapping mempty


data Config coord = Config
  { wids           :: WidMapping coord
      -- ^
      -- Maps a coord to a workspace id and a human-readable name
      -- Coords with equivalent images will share content
  , toNeighborhood :: coord -> [coord]
      -- ^ Computes which workspaces should be shown
  , toName         :: WorkspaceId -> String
      -- ^ Maps a workspace ID to human-readable name
  , toLabel        :: coord -> Maybe String
      -- ^ Computes the current label, eg a y-axis in a grid
  } deriving (Generic)

instance Semigroup (Config coord) where
  ca <> cb = cb

instance Ord coord => Default (Config coord) where
  def = Config
          { wids = mempty
          , toName = id
          , toNeighborhood = (:[])
          , toLabel = Just . const ""
          }

data Ix coord = Ix
  { config :: Config coord
  , coord  :: coord
  } deriving (Generic)

getIx
  :: forall coord
   . (ExtensionClass coord, Ord coord)
  => Proxy coord -> X (Ix coord)
getIx px = Ix <$> (fromMaybe def <$> XC.ask) <*> XS.get

getWid :: Ord coord => Config coord -> coord -> Maybe WorkspaceId
getWid config = flip Map.lookup (unWidMapping . wids $ config)

span :: (Ord x, Enum x) => (coord -> x) -> WidMapping coord -> [x]
span project (WidMapping wids) =
  let xs = project <$> Map.keys wids
  in case xs of
      [] -> []
      _  -> [minimum xs .. maximum xs]

pp :: Ord coord => Ix coord -> PP
pp ix = let

  activeWids :: [WorkspaceId]
  activeWids =
    (ix ^. #config . #toNeighborhood) (ix ^. #coord)
    & map (getWid (ix ^. #config))
    & catMaybes

  toName = ix ^. #config . #toName

  in def
      { ppCurrent = toName
      , ppHidden = toName
      , ppHiddenNoWindows = toName
      , ppSort = do
          sort <- (mkWsSort . pure) (compare `on` flip elemIndex activeWids)
          pure $ filter (tag >>> (`elem` activeWids))
                 >>> sort
      , ppOrder = \(workspaces : rest) -> (label <> workspaces) : rest
      }

  where

  label = fromMaybe "<err>" $ (ix ^. #config . #toLabel) (ix ^. #coord)

hook :: Typeable coord => Config coord -> XConfig l -> XConfig l
hook config = XC.once modifyXConfig config
  where

  modifyXConfig xConfig = xConfig { workspaces = calcWorkspaces (config ^. #wids) }

  calcWorkspaces :: WidMapping coord -> [WorkspaceId]
  calcWorkspaces = toList . unWidMapping

