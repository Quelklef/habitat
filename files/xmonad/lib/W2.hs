{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# OPTIONS_GHC -fwarn-unused-binds           #-}

module W2 where

import           Control.Category             ((>>>))
import           Control.Lens                 ((%~), (&), (.~), (^.))
import           Data.Function                (on)
import           Data.Generics.Labels         ()
import           Data.List                    (elemIndex, nub)
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


-- Allows a workspace to appear at multiple coordinates
--
-- To have two coordinates p1 and p2 share a workspace, assign
-- them the same value in in the Glue map. (Except Nothing)
--
-- nb. I much prefer the more definition
--   data Glue a = forall rep. Eq rep => Glue (a -> rep)
-- which allows general construction of equivalence relations
-- But we can't use that because we need stuff like
-- a Show Glue instance =(
newtype Glue = Glue (Map Coord String)
  deriving newtype (Monoid, Semigroup)

-- WANT: currently Glue is acting both to glue workspaces
--       together and to give them names. this should be
--       decoupled

-- Glue a column together
column :: W2Config -> Int -> String -> Glue
column (W2Config { yLabels }) x name =
  toIndices yLabels & foldMap (\y -> Glue $ Map.singleton (XY x y) name)


data Coord = XY { x :: Int, y :: Int }
  deriving (Eq, Ord, Generic)

(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n >= 0 && n < length xs = Just (xs !! n)
  | otherwise = Nothing

toIndices :: [a] -> [Int]
toIndices xs = take (length xs) [0..]


data W2Config = W2Config
  { glue    :: Glue
  , xLabels :: [String]
  , yLabels :: [String]
  } deriving (Generic)

instance Semigroup W2Config where
  ca <> cb = cb

instance Default W2Config where
  def = W2Config
    { glue = mempty
    , xLabels = show <$> [1 .. 5]
    , yLabels = show <$> [1 .. 5]
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


-- | Computes a workspace id from its coordinates
toWsName :: W2Config -> Coord -> Maybe WorkspaceId
toWsName (W2Config { xLabels, yLabels, glue = Glue glue }) (XY x y) =

  case (xLabels !? x, yLabels !? y) of

    (Just x', Just y') ->
      -- in-bounds
      case Map.lookup (XY x y) glue of
        Just name -> Just name
        Nothing   -> Just $ y' <> ":" <> x'

    _ ->
      -- out-of-bounds
      Nothing

getYLabel :: W2 -> String
getYLabel w2 =
  fromMaybe "<impossible>" $ (w2 ^. #config ^. #yLabels) !? (w2 ^. #state . #loc . #y)

mapCoord :: (Coord -> Coord) -> X ()
mapCoord f = do
  w2 <- getW2
  let w2' = w2 & #state . #loc %~ f
  let wsName = toWsName (config w2) (w2' ^. #state . #loc)
  case wsName of
    Nothing -> pure ()  -- moved out-of-bounds
    Just ws -> do
      XS.put (state w2')
      windows (W.greedyView ws)

incX, decX, incY, decY :: X ()
incX = mapCoord $ #x %~ (+1)
decX = mapCoord $ #x %~ (+(-1))
incY = mapCoord $ #y %~ (+1)
decY = mapCoord $ #y %~ (+(-1))

setX, setY :: Int -> X ()
setX x = mapCoord $ #x .~ x
setY y = mapCoord $ #y .~ y

moveWindowToX :: Int -> X ()
moveWindowToX x = do
  w2 <- getW2
  let y = w2 ^. #state . #loc . #y
  case toWsName (config w2) (XY x y) of
    Nothing   -> pure ()
    Just name -> windows (W.shift name)

augmentPP :: W2 -> (PP -> PP)
augmentPP w2 pp = let

  XY _ y = w2 ^. #state . #loc

  activeRow :: [WorkspaceId]
  activeRow =
    toIndices (w2 ^. #config . #xLabels)
    & map (\x -> XY x y)
    & map (toWsName (w2 ^. #config))
    & catMaybes

  -- WANT: this is pretty hacky :~/
  unformat :: String -> String
  unformat = reverse >>> takeWhile (/= ':') >>> reverse

  in pp
        { ppCurrent = unformat
        , ppHidden = unformat
        , ppHiddenNoWindows = const "..."
        , ppSort = do
            sort <- (mkWsSort . pure) (compare `on` flip elemIndex activeRow)
            pure $ filter (W.tag >>> (`elem` activeRow))
                   >>> sort
        }

withW2 :: W2Config -> XConfig l -> XConfig l
withW2 w2Config = XC.once modifyXConfig w2Config
  where

  modifyXConfig :: XConfig l -> XConfig l
  modifyXConfig xConfig = xConfig { workspaces = computeWorkspaceIds w2Config }

  computeWorkspaceIds :: W2Config -> [WorkspaceId]
  computeWorkspaceIds w2Config@(W2Config { xLabels, yLabels }) =
    (do
      y <- toIndices yLabels
      x <- toIndices xLabels
      let coord = XY x y
      pure $ toWsName w2Config coord
    )
    & catMaybes
    & nub  -- account for duplicate names due to gluing

