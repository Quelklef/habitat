{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module XMonad.WorkspaceLayouts.Cycle where

import           Prelude

import           Control.Lens                     ((%~), (.~), (^.))
import           Control.Monad.State              (execState)
import           Data.Generics.Labels             ()
import           GHC.Generics                     (Generic)
import qualified XMonad
import           XMonad                           hiding (config, state, trace,
                                                   workspaces)
import           XMonad.StackSet                  (greedyView, shift)

import           XMonad.WorkspaceLayouts.Core     (WorkspaceLayoutView (..))
import qualified XMonad.WorkspaceLayouts.OneState as St
import           XMonad.WorkspaceLayouts.OneState (OneState (..))
import           XMonad.WorkspaceLayouts.Util     (affineMod, (!%))



data Coord = Coord
  { offset   :: Int
  , position :: Int
  }
  deriving (Show, Eq, Ord, Generic)

data Config = Config
  { width      :: Int
  , workspaces :: [WorkspaceId]
  }
  deriving (Show, Generic)

data State = State
  { coord  :: Coord
  , config :: Config
  }
  deriving (Show, Generic)

instance OneState State where
  type Mod State = State -> State
  merge ma s = pure (ma s)
  defaultState = State
    { coord = Coord 0 0
    , config = Config 5 (single <$> ['a' .. 'j'])
    }
    where single = (:[])


data BoundsMode = Clamp | Wrap

move :: BoundsMode -> (Coord -> Coord) -> X ()
move mode f = do
  (coord', wid') <- calc mode f
  St.modify (#coord .~ coord' :: State -> State)
  windows (greedyView wid')

swap :: BoundsMode -> (Coord -> Coord) -> X ()
swap mode f = do
  (_, wid') <- calc mode f
  windows (shift wid')

calc :: BoundsMode -> (Coord -> Coord) -> X (Coord, WorkspaceId)
calc mode f = do
  State coord (Config { width, workspaces }) <- St.get
  let coord' = flip execState coord $ do
        modify f
        offset' <- (^. #offset) <$> get
        modify $ #position %~
          (let lo = offset' - width `div` 2
               hi = offset' + width `div` 2
          in case mode of
            Clamp -> max lo . min hi
            Wrap  -> affineMod (lo, hi))
  let wid = workspaces !% (coord' ^. #position)
  pure (coord', wid)


hook :: Config -> XConfig l -> XConfig l
hook config = St.once @State
  (\xc -> xc { XMonad.workspaces = config ^. #workspaces })
  (\state -> state { config = config })

getView :: X WorkspaceLayoutView
getView = do
  State (Coord { offset }) (Config { width, workspaces }) <- St.get
  pure $ WSLView
    { toName = id
    , label = ""
    , neighborhood =
            (do pos <- [offset - width `div` 2 .. offset + width `div` 2]
                pure $ workspaces !% (pos `mod` length workspaces)
            )
    }

