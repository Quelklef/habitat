{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wall -Wwarn #-}

module XMonad.WorkspaceLayouts.Core where

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
import           Data.Monoid                  (Endo (..), appEndo)
import           GHC.Generics                 (Generic)
import qualified XMonad
import           XMonad                       hiding (config, modify, state,
                                               trace, workspaces)
import           XMonad.Hooks.StatusBar.PP    (PP (..))
import           XMonad.StackSet              (tag)
import qualified XMonad.Util.ExtensibleConf   as XC
import qualified XMonad.Util.ExtensibleState  as XS
import           XMonad.Util.WorkspaceCompare (mkWsSort)


-- |
--
-- Encompasses information needed to render a workspace layout
data WorkspaceLayoutView = WSLView
  { label        :: String
  , neighborhood :: [WorkspaceId]
  , toName       :: WorkspaceId -> String
  }


render :: WorkspaceLayoutView -> PP
render (WSLView { neighborhood, toName, label }) =

  withLabel . withNameTransform . withNeighborhood $ def

  where

  withNameTransform pp = pp
    { ppCurrent = toName
    , ppHidden = toName
    , ppHiddenNoWindows = toName
    }

  withNeighborhood pp = pp
    { ppSort = do
        sort <- (mkWsSort . pure) (compare `on` flip elemIndex neighborhood)
        pure $ filter (tag >>> (`elem` neighborhood)) >>> sort
    }

  withLabel pp = pp
    { ppOrder = \(workspaces : rest) -> (label <> workspaces) : rest
    }


-- Doubly-inclusive
affineMod :: (Ord a, Num a) => (a, a) -> (a -> a)
affineMod range@(lo, hi) x
  | x > hi = affineMod range (x - (hi - lo + 1))
  | x < lo = affineMod range (x + (hi - lo + 1))
  | otherwise = x

