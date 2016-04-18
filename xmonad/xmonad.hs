import XMonad
import XMonad.Actions.OnScreen
import XMonad.Actions.SpawnOn
import XMonad.Actions.Warp
import XMonad.Layout.Named
import XMonad.Layout.IM
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.WorkspaceCompare
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import System.IO
import System.Exit

import Data.List -- use for find
import Data.Ord -- use for comparing
import Data.Maybe -- use for fromjust
import Data.Ratio -- used for 1%2

import Graphics.X11.ExtraTypes.XF86

-- My Modules
import qualified ScreenManager as SM

--Basic Settings
myBorderWidth = 1

-- Workspace names and Physical Screen order
--myWorkspaces = ["一", "二","三","四","五","六","七","八","九"]
--myWorkspaces = ["1", "2","3","4","5","6","7","8","9"]
--myWorkspaces = ["dev", "ref","www","chat","","rf2","ide","msg","vid"]
myWorkspaces = ["1", "2","3","4","5","6","7","8","9"]

screenSettings = SM.buildScreenSettings
    [ ("C", (0, xK_d, "1")),
      ("L", (1, xK_s, "5")),
      ("R", (2, xK_f, "2")) ]
    ["L","C","R"]

xmobarScreen = "C"

-- Program Definitons
myPrograms = M.fromList
    [ ("terminal", "urxvtc")
    , ("browser", "chromium")
    , ("browser-alt", "firefox")
    , ("chat", "urxvt -e sh -c 'weechat'")
    , ("music", "urxvt -e sh -c 'ncmpcpp'")
    , ("media", "vlc")
    --, ("lock", "i3lock -t -i /home/scott/media/images/locked.png; spotify_remote.sh pause")
    , ("lock", "xscreensaver-command -lock")
    , ("media-pauseplay", "spotify_remote.sh playpause")
    , ("media-previous", "spotify_remote.sh previous")
    , ("media-next", "spotify_remote.sh next")
    ]

getProgram :: String -> String
getProgram programName = fromJust $ M.lookup programName myPrograms

-- Program Automatic Workspace Mappings
myManageHooks = composeAll
    [
        className   =? "Vlc"        --> doShift (myWorkspaces !! 8),
        className   =? "Darktable"  --> doShift (myWorkspaces !! 6)
    ]

-- Startup Applications

workspaceLookup = SM.lookupScreenWorkspace screenSettings
screenIdLookup = SM.lookupScreenId screenSettings

myStartupHook :: X ()
myStartupHook = do
    windows (greedyViewOnScreen (screenIdLookup "L") (workspaceLookup "L") );
    windows (greedyViewOnScreen (screenIdLookup "C") (workspaceLookup "C") );
    windows (greedyViewOnScreen (screenIdLookup "R") (workspaceLookup "R") );
    screenWorkspace 0 >>= flip whenJust (windows . W.view);
--    spawnOn "1" $ getProgram "browser";

----------------------------------------------------------------------------------------------------
-- SCRATCHPADS
----------------------------------------------------------------------------------------------------
scratchpads = [
    NS "spotify" "spotify" (className =? "Spotify")
        (customFloating $ W.RationalRect (0.1) (0.1) (0.8) (0.8)),
    NS "keepass" "keepass" (className =? "KeePass2")
        (defaultFloating),
    NS "pavucontrol" "pavucontrol" (className =? "Pavucontrol")
        (customFloating $ W.RationalRect (0.1) (0.1) (0.8) (0.8))
    ]

----------------------------------------------------------------------------------------------------
-- LAYOUTS
----------------------------------------------------------------------------------------------------
gapSize = 11
increment = (2/100)

tallLayout       = named "T" $ avoidStruts $ Tall 1 increment (2/(1+(toRational(sqrt(5)::Double))))
tallEqualLayout  = named "E" $ avoidStruts $ Tall 1 increment (1/2)
tallGapLayout    = named "t" $ avoidStruts $ gaps [(U, gapSize), (D, gapSize), (L, gapSize), (R, gapSize)] $ spacing gapSize $ Tall 1 increment (2/(1+(toRational(sqrt(5)::Double))))
wideLayout       = named "W" $ avoidStruts $ Mirror (Tall 1 increment (2/(1+(toRational(sqrt(5)::Double)))))
singleLayout     = named "S" $ avoidStruts $ noBorders Full
tabbedLayout     = named "B" $ avoidStruts $ simpleTabbed
fullscreenLayout = named "F" $ noBorders Full
--threeLayout      = named "3" $ avoidStruts $ ThreeCol 1 increment (1/2)
threeMLayout     = named "M" $ avoidStruts $ ThreeColMid 1 increment (1/2)

myLayoutHook = onWorkspaces [myWorkspaces !! 8] fullscreenLayout
             $ onWorkspaces [myWorkspaces !! 3] tabbedLayout
             $ tallGapLayout ||| tallLayout
                             ||| tallEqualLayout
                             ||| singleLayout
--                           ||| threeLayout
                             ||| tabbedLayout
                             ||| threeMLayout
                             ||| wideLayout

----------------------------------------------------------------------------------------------------
-- KEY BINDINGS
----------------------------------------------------------------------------------------------------
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ ((modMask .|. shiftMask,   xK_Return), spawnHere $ XMonad.terminal conf)                                   -- spawn terminal
  , ((modMask,                 xK_w),      spawnHere $ getProgram "browser")                                   -- run browser
  , ((modMask,                 xK_z),      warpToWindow (1%2) (1%2))                                           -- move mouse ptr
  , ((mod4Mask,                xK_i),      namedScratchpadAction scratchpads "spotify")                        -- open spotify
  , ((mod4Mask,                xK_u),      namedScratchpadAction scratchpads "keepass")                        -- open keepass
  , ((mod4Mask,                xK_p),      namedScratchpadAction scratchpads "pavucontrol")                    -- open keepass
  , ((modMask,                 xK_p),      spawnHere "dmenu_custom")                                           -- dmenu_run
  , ((mod4Mask,                xK_c),      spawnHere $ getProgram "media-pauseplay")                           -- spotify play
  , ((mod4Mask,                xK_x),      spawnHere $ getProgram "media-previous")                            -- spotify prev
  , ((mod4Mask,                xK_v),      spawnHere $ getProgram "media-next")                                -- spotify next
  , ((modMask .|. shiftMask,   xK_p),      spawnHere "scrot -e 'mv $f ~/media/images/scrots'")                 -- fullscreen screenshot
  , ((modMask .|. shiftMask,   xK_w),      spawnHere $ getProgram "browser-alt")                               -- run browser-alt
  , ((modMask .|. controlMask, xK_l),      spawnHere $ getProgram "lock")                                      -- lock screen
  , ((modMask .|. controlMask, xK_p),      spawnHere "sleep 0.2; scrot -s -e 'mv $f ~/media/images/scrots'")   -- selection screenshot
  , ((modMask .|. controlMask, xK_Up),     spawnHere "xrandr --output eDP1 --rotate normal")
  , ((modMask .|. controlMask, xK_Down),   spawnHere "xrandr --output eDP1 --rotate inverted")
  , ((modMask .|. controlMask, xK_Left),   spawnHere "xrandr --output eDP1 --rotate right")
  , ((modMask .|. controlMask, xK_Right),  spawnHere "xrandr --output eDP1 --rotate left")

  -- http://xmonad.org/xmonad-docs/X11/Graphics-X11-ExtraTypes-XF86.html
  , ((0, xF86XK_AudioMute),           spawnHere "amixer -q sset 'Master' toggle")
  , ((0, xF86XK_AudioLowerVolume),    spawnHere "amixer -q sset 'Master' 10%-")
  , ((0, xF86XK_AudioRaiseVolume),    spawnHere "amixer -q sset 'Master' 10%+")
  , ((0, xF86XK_MonBrightnessUp),     spawnHere "xbacklight -inc 10")
  , ((0, xF86XK_MonBrightnessDown),   spawnHere "xbacklight -dec 10")

  -- standard XMonad keybindings
  , ((modMask .|. shiftMask,   xK_c), kill)
  , ((modMask,                 xK_space), sendMessage NextLayout)
  , ((modMask .|. shiftMask,   xK_space), setLayout $ XMonad.layoutHook conf)
  , ((modMask,                 xK_n), refresh)
  , ((modMask,                 xK_Tab), windows W.focusDown)
  , ((modMask,                 xK_j), windows W.focusDown)
  , ((modMask,                 xK_k), windows W.focusUp)
  , ((modMask,                 xK_m), windows W.focusMaster)
  , ((modMask,                 xK_Return), windows W.swapMaster)
  , ((modMask .|. shiftMask,   xK_j), windows W.swapDown  )
  , ((modMask .|. shiftMask,   xK_k), windows W.swapUp    )
  , ((modMask,                 xK_h), sendMessage Shrink)
  , ((modMask,                 xK_l), sendMessage Expand)
  , ((modMask,                 xK_t), withFocused $ windows . W.sink) -- snap window into tiling
  , ((modMask,                 xK_comma), sendMessage (IncMasterN 1)) -- add more to master
  , ((modMask,                 xK_period), sendMessage (IncMasterN (-1))) -- remove some from master
  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip (SM.getScreenKeys screenSettings) myScreens
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

--conkyPP :: PP -> X ()
--conkyPP prettyPrint = (ppOutput prettyPrint) (dynamicLogString prettyPrint)

shortenNoTrunc :: Int -> String -> String
shortenNoTrunc n xs | length xs < n = xs
                    | otherwise     = take (n) xs

appendText :: String -> String -> String
appendText sa sb = sb ++ sa

prependText :: String -> String -> String
prependText sa sb = sa ++ sb

myLogHook h = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ defaultPP
     {
      ppOutput          = hPutStrLn h . prependText "^bg(#282A2E)" . appendText "^fg(#282A2E)^bg()^i(/home/scott/theme/arrow32left.xbm)"
    --, ppTitle           = ("^bg(#324C80) ^i(/home/scott/.dzen_icons/pacman.xbm)" ++ ) . shortenNoTrunc 0 
    --, ppTitle           = dzenColor "#999999" "" . shorten 200
    , ppCurrent         = dzenColor "#81A2BE" "" . pad
    , ppVisible         = dzenColor "#81A2BE" "" . pad
    , ppWsSep           = "^bg(#282A2E)" -- set the background color
    , ppSep             = ""
    , ppHiddenNoWindows = dzenColor "#000000" "" . pad
    , ppHidden          = dzenColor "#B5BD68" "" . pad
    , ppOrder           = \(ws:l:t:_) -> [ws]
    }

sidebarwidth = 500
sidebarheight = 24
barFont = "\"Source Han Sans:size=8\""
trayerSize = 120

startX = 0
startY = 0

main = do
    dzproc <- spawnPipe $ ("/bin/dzen2 -x 0 -y 0 -h " ++ (show sidebarheight) ++ " -w " ++ (show sidebarwidth) ++ " -fg '#FFFFFF' -bg '#1a1a1a' -ta l -fn " ++ barFont)

    -- right
    spawn $ (
        "conky --config /home/scott/etc/conky/dzen-right | /bin/dzen2" ++
        " -x " ++ (show (1920 - sidebarwidth)) ++
        " -y 0 " ++
        " -h " ++ (show sidebarheight) ++
        " -w " ++ (show (sidebarwidth - trayerSize)) ++
        " -fg '#FFFFFF'" ++
        " -bg '#1a1a1a'" ++
        " -ta r" ++
        " -fn " ++ barFont)

    -- center
    spawn $ (
        "conky --config /home/scott/etc/conky/dzen-center | /bin/dzen2" ++
        " -x " ++ (show (sidebarwidth)) ++
        " -y 0 " ++
        " -h " ++ (show (sidebarheight)) ++
        " -w " ++ (show (1920 - 2 * sidebarwidth)) ++
        " -fg '#FFFFFF'" ++
        " -bg '#1a1a1a'" ++
        " -ta c" ++
        " -fn " ++ barFont)

    -- trayer on the right
    spawn $ (
        "trayer --edge top --align right --widthtype pixel" ++
        " --width " ++ (show (trayerSize)) ++
        " --expand true " ++
        " --SetDockType true " ++
        " --transparent true " ++
        " --alpha 0 " ++
        " --heighttype pixel" ++
        " --height " ++ (show sidebarheight) ++
        " --padding 5 " ++
        " --tint 0x282A2E " ++
        " --margin 0 &")

    xmonad $ defaultConfig {
         manageHook = myManageHooks <+> manageSpawn <+> manageDocks <+> namedScratchpadManageHook scratchpads <+> manageHook defaultConfig
        , startupHook = myStartupHook
        , layoutHook = myLayoutHook
        , keys = myKeys
        , logHook = myLogHook dzproc
        , borderWidth        = myBorderWidth
        , terminal           = getProgram "terminal"
        , workspaces         = myWorkspaces
        , normalBorderColor = "#000000"
        , focusedBorderColor = "#81A2BE"
    }

-- Custom sort for Xmobar that will match the physical layout of your montiors as defined by myScreens
myScreens = SM.getScreenIds screenSettings

myScreenSortVals = [ snd x | x <- (sort $ zip myScreens [1..])]
myWorkspaceCompare :: X WorkspaceCompare
myWorkspaceCompare = do
    w <- gets windowset
    return $ \ a b -> case (isOnScreen a w, isOnScreen b w) of
        (True, True)   -> compare (myScreenSortVals !! (fromIntegral ((tagToSid (onScreen w)) a))) (myScreenSortVals !! (fromIntegral ((tagToSid (onScreen w)) b)))
        (False, False) -> compare a b
        (True, False)  -> LT
        (False, True)  -> GT
    where
        onScreen w =  W.current w : W.visible w
        isOnScreen a w  = a `elem` map (W.tag . W.workspace) (onScreen w)
        tagToSid s x = W.screen $ fromJust $ find ((== x) . W.tag . W.workspace) s


