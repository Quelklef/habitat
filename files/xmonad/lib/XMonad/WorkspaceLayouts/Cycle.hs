{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module XMonad.WorkspaceLayouts.Cycle where

import           Prelude

import           Control.Lens                 ((%~), (^.))
import           Control.Monad.State          (execState)
import           Data.Generics.Labels         ()
import           Data.Maybe                   (fromMaybe)
import           GHC.Generics                 (Generic)
import qualified XMonad
import           XMonad                       hiding (config, state, trace,
                                               workspaces)
import           XMonad.StackSet              (greedyView, shift)
import qualified XMonad.Util.ExtensibleConf   as XC
import qualified XMonad.Util.ExtensibleState  as XS

import           XMonad.WorkspaceLayouts.Core (WorkspaceLayoutView (..),
                                               affineMod)

import           Debug.Trace



data Coord = Coord
  { offset   :: Int
  , position :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance ExtensionClass Coord where
  initialValue = Coord 0 0

data Config = Config
  { width      :: Int
  , workspaces :: [WorkspaceId]
  }
  deriving (Show, Generic)

instance Semigroup Config where
  _ <> c = c

instance Default Config where
  def = Config 5 (single <$> ['a' .. 'j'])
    where single = (:[])


data BoundsMode = Clamp | Wrap

move :: BoundsMode -> (Coord -> Coord) -> X ()
move mode f = do
  (coord', wid') <- calc mode f
  XS.put coord'
  windows (greedyView wid')

swap :: BoundsMode -> (Coord -> Coord) -> X ()
swap mode f = do
  (_, wid') <- calc mode f
  windows (shift wid')

calc :: BoundsMode -> (Coord -> Coord) -> X (Coord, WorkspaceId)
calc mode f = do
  (coord, Config { width, workspaces }) <- getBoth
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
hook config = XC.once endo config
  where
  endo xc = xc { XMonad.workspaces = config ^. #workspaces }

getView :: X WorkspaceLayoutView
getView = do
  (Coord { offset }, Config { width, workspaces }) <- traceShowId <$> getBoth
  pure $ WSLView
    { toName = id
    , label = ""
    , neighborhood =
            (do pos <- [offset - width `div` 2 .. offset + width `div` 2]
                pure $ workspaces !% (pos `mod` length workspaces)
            )
    }

(!%) :: [a] -> Int -> a
xs !% n = xs !! (n `mod` length xs)

getBoth :: (Default conf, Typeable conf, ExtensionClass state) => X (state, conf)
getBoth = (,) <$> XS.get <*> (fromMaybe def <$> XC.ask)
