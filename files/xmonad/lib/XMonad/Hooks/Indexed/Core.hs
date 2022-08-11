{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wall -Wwarn #-}

module XMonad.Hooks.Indexed.Core where

import           Prelude                      hiding (span)

import           Control.Category             ((>>>))
import           Control.Lens                 (view, (%~), (&), (.~), (^.))
import           Data.Foldable                (fold, toList)
import           Data.Function                (on)
import           Data.Functor                 (($>))
import           Data.Generics.Labels         ()
import           Data.List                    (elemIndex, intercalate, nub,
                                               sort)
import           Data.List.Split              (splitOn)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (catMaybes, fromMaybe, isJust)
import           GHC.Generics                 (Generic)
import           XMonad                       hiding (config, state, trace)
import           XMonad.Hooks.StatusBar.PP    (PP (..))
import           XMonad.StackSet              (tag)
import qualified XMonad.Util.ExtensibleConf   as XC
import qualified XMonad.Util.ExtensibleState  as XS
import           XMonad.Util.WorkspaceCompare (mkWsSort)


newtype WidMapping coord = WidMapping { unWidMapping :: Map coord WorkspaceId }
  deriving (Show, Eq, Generic)

instance Ord coord => Semigroup (WidMapping coord) where
  WidMapping ma <> WidMapping mb = WidMapping (mb <> ma)  -- right-biased

instance Ord coord => Monoid (WidMapping coord) where
  mempty = WidMapping mempty

range :: Ord x => (coord -> x) -> WidMapping coord -> Maybe (x, x)
range proj (WidMapping wids) =
  let xs = proj <$> Map.keys wids
  in case xs of
      [] -> Nothing
      _  -> Just (minimum xs, maximum xs)

span :: (Ord x, Enum x) => (coord -> x) -> WidMapping coord -> [x]
span proj (WidMapping wids) =
  let xs = proj <$> Map.keys wids
  in case xs of
      [] -> []
      _  -> [minimum xs .. maximum xs]


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
  _ <> c = c

instance Ord coord => Default (Config coord) where
  def = Config
          { wids = mempty
          , toName = id
          , toNeighborhood = (:[])
          , toLabel = Just . const ""
          }

getBoth :: (Default conf, Typeable conf, ExtensionClass state) => X (state, conf)
getBoth = (,) <$> XS.get <*> (fromMaybe def <$> XC.ask)

getWid :: Ord coord => Config coord -> coord -> Maybe WorkspaceId
getWid config = flip Map.lookup (unWidMapping . wids $ config)

pp :: Ord coord => Config coord -> coord -> PP
pp config coord = let

  activeWids :: [WorkspaceId]
  activeWids =
    (config ^. #toNeighborhood) coord
    & map (getWid config)
    & catMaybes

  toName = config ^. #toName

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

  label = fromMaybe "<err>" $ (config ^. #toLabel) coord

modifyXConfig :: Typeable coord => Config coord -> XConfig l -> XConfig l
modifyXConfig config xConfig = xConfig { workspaces = calcWorkspaces (config ^. #wids) }
  where

  calcWorkspaces :: WidMapping coord -> [WorkspaceId]
  calcWorkspaces =
    unWidMapping
    >>> toList
    >>> nub  -- account for two coordinates pointing to the same workspace

