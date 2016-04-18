module ScreenManager
( ScreenSettings(..)
, SettingsList
, buildScreenSettings

, getScreenIds
, getScreenKeys
, getScreenWorkspaces

, lookupScreenId
, lookupScreenKey
, lookupScreenWorkspace
) where

import XMonad
import Data.Maybe
import qualified Data.Map as M

-- Types
data ScreenSettings = ScreenSettings { 
    index :: ScreenId,
    key :: KeySym,
    startingWorkspace :: String
} deriving (Show)

data SettingsList = SettingsList (M.Map String ScreenSettings) [String]

-- Public Functions

buildScreenSettings :: [ (String, (ScreenId, KeySym, String)) ] -> [String] -> SettingsList
buildScreenSettings sl order = SettingsList (M.fromList $ map buildSingleSettings sl ) order

getScreenIds :: SettingsList -> [ScreenId]
getScreenIds sl = settingExtractor sl index

getScreenKeys :: SettingsList -> [KeySym]
getScreenKeys sl = settingExtractor sl key  

getScreenWorkspaces :: SettingsList -> [String]
getScreenWorkspaces sl = settingExtractor sl startingWorkspace

lookupScreenId :: SettingsList -> String -> ScreenId
lookupScreenId sl screen_name = settingsLookup sl screen_name index

lookupScreenKey :: SettingsList -> String -> KeySym
lookupScreenKey sl screen_name = settingsLookup sl screen_name key

lookupScreenWorkspace :: SettingsList -> String -> String
lookupScreenWorkspace sl screen_name = settingsLookup sl screen_name startingWorkspace

-- Private Functions

buildSingleSettings :: (String, (ScreenId, KeySym, String)) -> (String, ScreenSettings)
buildSingleSettings (name, (s_id, s_key, s_ws)) = (name, ScreenSettings {index = s_id, key = s_key, startingWorkspace = s_ws})

settingExtractor :: SettingsList -> (ScreenSettings -> a) -> [a]
settingExtractor (SettingsList sl order) f = map (\x -> f $ fromJust $ M.lookup x sl) order

settingsLookup :: SettingsList -> String -> (ScreenSettings -> a) -> a
settingsLookup (SettingsList sl order) screen_name f = f $ fromJust $ M.lookup screen_name sl
