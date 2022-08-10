{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS_GHC -fwarn-unused-binds     #-}

{- | Two-dimensional workspaces for XMonad -}

module W2 where

import           Control.Category             ((>>>))
import           Control.Lens                 (view, (%~), (&), (.~), (^.))
import           Data.Foldable                (fold)
import           Data.Function                (on)
import           Data.Generics.Labels         ()
import           Data.List                    (elemIndex, intercalate, nub)
import           Data.List.Split              (splitOn)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (catMaybes, fromMaybe)
import           GHC.Generics                 (Generic)
import           XMonad                       hiding (config, state)
import           XMonad.Hooks.StatusBar.PP    (PP (..))
import qualified XMonad.StackSet              as W
import qualified XMonad.Util.ExtensibleConf   as XC
import qualified XMonad.Util.ExtensibleState  as XS
import           XMonad.Util.WorkspaceCompare (mkWsSort)


data Coord = XY { x :: Int, y :: Int }
  deriving (Eq, Ord, Generic)

(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n >= 0 && n < length xs = Just (xs !! n)
  | otherwise = Nothing

indices :: [a] -> [Int]
indices xs = take (length xs) [0..]


-- |
-- Used to give custom names to workspace cells.
-- If two cells have the same name, they will share content!
--
-- See @column@ and @group@
newtype Names = Names { unNames :: Map Coord String }
  deriving (Semigroup, Monoid)

-- | Glue together a column of workspaces
column :: W2Config -> Int -> String -> Names
column config x name =
  Names . fold $ do
    y <- indices (config ^. #yLabels)
    pure $ Map.singleton (XY x y) name

-- | Glue together a group of workspaces
group :: Foldable f => f Coord -> String -> Names
group coords name = Names $ coords & foldMap (flip Map.singleton name)


data W2Config = W2Config
  { names   :: Names
  , xLabels :: [String]
  , yLabels :: [String]
  } deriving (Generic)

instance Semigroup W2Config where
  ca <> cb = cb

instance Default W2Config where
  def = W2Config
    { xLabels = show <$> [1 .. 5]
    , yLabels = show <$> [1 .. 5]
    , names = mempty
    }

data W2State = W2State
  { loc :: Coord
  } deriving (Generic)

instance Default W2State where
  def = W2State { loc = XY 0 0 }

instance ExtensionClass W2State where
  initialValue = def


data W2 = W2
  { config :: W2Config
  , state  :: W2State
  } deriving (Generic)

-- | Get the entire W2 state
getW2 :: X W2
getW2 = W2 <$> (fromMaybe def <$> XC.ask) <*> XS.get


-- | Computes a ws id from its coordinates
toWorkspaceId :: W2Config -> Coord -> Maybe WorkspaceId
toWorkspaceId (W2Config { xLabels, yLabels, names }) (XY x y) =

  case (xLabels !? x, yLabels !? y) of

    (Just x', Just y') ->
      -- in-bounds
      case Map.lookup (XY x y) (unNames names) of
        Just name -> Just $ ":" <> name
        Nothing   -> Just $ y' <> "/" <> x' <> ":" <> x'

    _ ->
      -- out-of-bounds
      Nothing

wsIdToWsName :: WorkspaceId -> String
wsIdToWsName = splitOn ":" >>> drop 1 >>> intercalate ":"

-- | Get the label of the currently-selected row
getYLabel :: W2 -> String
getYLabel w2 =
  fromMaybe "<error>" $ (w2 ^. #config ^. #yLabels) !? (w2 ^. #state . #loc . #y)

-- | Modify the current coordinate
mapCoord :: (Coord -> Coord) -> X ()
mapCoord f = do
  w2 <- getW2
  let w2' = w2 & #state . #loc %~ f
  let wsName = toWorkspaceId (config w2) (w2' ^. #state . #loc)
  case wsName of
    Nothing -> pure ()  -- moved out-of-bounds
    Just ws -> do
      XS.put (state w2')
      windows (W.greedyView ws)

-- | Modify the current coordinate
incX, decX, incY, decY :: X ()
incX = mapCoord $ #x %~ (+1)
decX = mapCoord $ #x %~ (+(-1))
incY = mapCoord $ #y %~ (+1)
decY = mapCoord $ #y %~ (+(-1))

-- | Modify the current coordinate
setX, setY :: Int -> X ()
setX x = mapCoord $ #x .~ x
setY y = mapCoord $ #y .~ y

-- | Move the currently-selected XMonad window to the given x coordinate
moveWindowToX :: Int -> X ()
moveWindowToX x = do
  w2 <- getW2
  let y = w2 ^. #state . #loc . #y
  case toWorkspaceId (config w2) (XY x y) of
    Nothing   -> pure ()
    Just name -> windows (W.shift name)

-- |
--
-- Modify a PP to work with W2
--
-- This overwrites:
--
-- * ppCurrent and ppHidden -- if you overwrite these again, you should
--   wrap them, ala @pp' = pp { ppCurrent = (<> "!") . ppCurrent pp }@
--
-- * ppHiddenNoWindows -- which you are free to overwrite
--
-- * ppSort -- which you should probably leave alone
augmentPP :: W2 -> (PP -> PP)
augmentPP w2 = let

  XY _ y = w2 ^. #state . #loc

  activeRow :: [WorkspaceId]
  activeRow =
    indices (w2 ^. #config . #xLabels)
    & map (\x -> XY x y)
    & map (toWorkspaceId (w2 ^. #config))
    & catMaybes

  in \pp -> pp
        { ppCurrent = wsIdToWsName
        , ppHidden = wsIdToWsName
        , ppHiddenNoWindows = const "..."
        , ppSort = do
            sort <- (mkWsSort . pure) (compare `on` flip elemIndex activeRow)
            pure $ filter (W.tag >>> (`elem` activeRow))
                   >>> sort
        }

-- | Modify an XConfig to use W2
withW2 :: W2Config -> XConfig l -> XConfig l
withW2 w2Config = XC.once modifyXConfig w2Config
  where

  modifyXConfig :: XConfig l -> XConfig l
  modifyXConfig xConfig = xConfig { workspaces = computeWorkspaceIds w2Config }

  computeWorkspaceIds :: W2Config -> [WorkspaceId]
  computeWorkspaceIds w2Config@(W2Config { xLabels, yLabels }) =
    (do
      y <- indices yLabels
      x <- indices xLabels
      let coord = XY x y
      pure $ toWorkspaceId w2Config coord
    )
    & catMaybes
    & nub  -- account for duplicate workspace names

