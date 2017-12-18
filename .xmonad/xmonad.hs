import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import System.IO
import System.Exit
import Data.Maybe
import qualified Data.Map               as M
import qualified XMonad.StackSet        as W

--spawnTerm :: X ()
--spawnTerm = do
--  windowID <- (fromMaybe 0 . W.peek . windowset) <$> get
--  dir <- withDisplay (\dpy -> io $ getIconName dpy windowID)
--  spawn $ "cd " ++ tail (dropWhile (/= ':') dir) ++ "; urxvt"
--  --spawn $ "echo \"" ++ tail (dropWhile (/= ':') dir) ++ "\" | xmessage -file -"


--spawnTerm :: X ()
--spawnTerm = do
--        windowID <- (W.peek . windowset) <$> get
--        case windowID of
--                Just winID -> do
--                        dir <- withDisplay (\dpy -> io $ getIconName dpy winID)
--                        --spawn $ "echo \"" ++ tail (dropWhile (/= ':') dir) ++ "\" | xmessage -file -"
--                        --spawn $ "cd " ++ (dropWhile (/= ':') dir) ++ "; uxrvt"
--                        spawn "/usr/bin/urxvt"
--                Nothing -> spawn "/usr/bin/urxvt"


-- Border Colors
myNormalBorderColor = "#585858"
myFocusedBorderColor = "#d70000"


-- Gap Widths
gapwidth = 4
gwU = 8
gwD = 8
gwL = 8
gwR = 8

-- Launchers
myBrowser       = "/usr/bin/firefox"
myTerminal      = "/usr/bin/urxvt"      
myLauncher      = "exe=`dmenu_path | dmenu` && eval \"exec $exe\""
myTrello        = "/usr/bin/surf www.trello.com"

-- Workspaces
myWorkspaces = ["1:term","2:web", "3:slack", "4:ranger", "5:trello", "6:sys"] ++ map show [7..9]

-- Layouts
myLayouts = tiled ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

--Gaps
myLayoutHook = spacing gapwidth $ gaps [(U, gwU),(D, gwD),(L, gwL),(R, gwR)] $ myLayouts

-- Window Rules
myManageHook = composeAll
        [ className =? "firefox"        --> doShift "2:web"
        , className =? "slack"          --> doShift "3:slack"
        , className =? "stalonetray"    --> doIgnore
        , manageDocks  
        ]

-- Keybindings
myKeys = \c -> mkKeymap c $ 
        ----- Custom Keys -----

        -- Launch Terminal
        [ (("M-<Return>")       , spawn myTerminal )

        -- Launch Browser
        , (("M-\\")             , spawn myBrowser)

        -- Launch DMenu
        , (("M-p")              , spawn myLauncher)

        -- Launch Trello
        , (("M-o")              , spawn myTrello)
        
        -- Close focused window.
        , (("M-<Backspace>")    , kill)

        -- Mute/Unmute amixer
        , (("<XF86AudioMute>")  , spawn "amixer -D pulse set Master 1+ toggle")

        -- Increase/decrease amixer volume
        , (("<XF86AudioRaiseVolume>")     , spawn "amixer set Master 10%+")
        , (("<XF86AudioLowerVolume>")     , spawn "amixer set Master 10%-")

        ----- Standard Xmonad Keys -----
        
        --- System:
        -- Restart Xmonad
        , (("M-q")              , restart "xmonad" True) 

        -- Quit xmonad.
        , (("M-S-q")            , io (exitWith ExitSuccess))

        --- Windows: 

        -- Move focus to the next window.
        , (("M-j")              , windows W.focusDown)
        
        -- Move focus to the previous window.
        , (("M-k")              , windows W.focusUp )

        -- Swap the focused window with the next window.
        , (("M-S-j")            , windows W.swapDown  )
        
        -- Swap the focused window with the previous window.
        , (("M-S-k")            , windows W.swapUp )

        -- Shrink the master area.
        , (("M-h")              , sendMessage Shrink)
        
        -- Expand the master area.
        , (("M-l")              , sendMessage Expand)

        -- Cycle through the available layout algorithms.
        , (("M-<Space>")        , sendMessage NextLayout) 

        -- Push window back into tiling
        , (("M-t")              , withFocused $ windows . W.sink)

        ]

        ++

        -- mod-[1..9], Switch to workspace N
        -- mod-shift-[1..9], Move client to workspace N
        [ (m ++ i, windows $ f j)
            | (i, j) <- zip (map show [1..9]) (XMonad.workspaces c)
            , (m, f) <- [("M-", W.greedyView), ("M-S-", W.shift)] --Shift wndw to ws
        ]

        

main = do
        xmproc <- spawnPipe "/home/solomon/.cabal-sandbox/bin/xmobar ~/.xmobarrc"
        xmonad $ docks def
                { layoutHook            = avoidStruts $ myLayoutHook
                , manageHook            = manageHook def <+> myManageHook 
                , logHook               = dynamicLogWithPP xmobarPP
                                                { ppOutput      = hPutStrLn xmproc
                                                , ppTitle       = xmobarColor "green" "" . shorten 150
                                                }
                , modMask               = mod4Mask
                , keys                  = myKeys
                , workspaces            = myWorkspaces
                , normalBorderColor     = myNormalBorderColor
                , focusedBorderColor    = myFocusedBorderColor
                }

