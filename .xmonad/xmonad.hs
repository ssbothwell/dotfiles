import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import System.IO
import System.Exit
import qualified Data.Map       as M
import qualified XMonad.StackSet as W

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
myBrowser = "/usr/bin/firefox"

-- Workspaces
myWorkspaces = ["1:term","2:web", "3:slack"] ++ map show [4..9]

-- Window Rules
myManageHook = composeAll
        [ className =? "firefox"        --> doShift "2:web"
        , className =? "slack"          --> doShift "3:slack"
        , className =? "stalonetray"    --> doIgnore
        , manageDocks  
        ]

-- Keybindings
myKeyss = \c -> mkKeymap c $ 
        ----- Custom Keys -----

        -- Launch Terminal
        [ (("M-<Return>")       , spawn $ XMonad.terminal c)

        -- Launch Browser
        , (("M-\\")            , spawn myBrowser)

        -- Close focused window.
        , (("M-<Backspace>")            , kill)

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

        ]

        ++

        -- mod-[1..9], Switch to workspace N
        -- mod-shift-[1..9], Move client to workspace N
        [ (m ++ i, windows $ f j)
            | (i, j) <- zip (map show [1..9]) (XMonad.workspaces c)
            , (m, f) <- [("M-", W.greedyView), ("M-S-", W.shift)] --Shift wndw to ws
        ]

        

main = do
        xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
        xmonad $ docks def
                { layoutHook            = avoidStruts $ myLayout
                , manageHook            = manageHook def <+> myManageHook 
                , logHook               = dynamicLogWithPP xmobarPP
                                                { ppOutput      = hPutStrLn xmproc
                                                , ppTitle       = xmobarColor "green" "" . shorten 150
                                                }
                , modMask               = mod4Mask
                --, keys                  = \c -> myKeys c `M.union` keys def c
                , keys                  = myKeyss
                , workspaces            = myWorkspaces
                , normalBorderColor     = myNormalBorderColor
                , focusedBorderColor    = myFocusedBorderColor
                }

myLayout = spacing gapwidth $ gaps [(U, gwU),(D, gwD),(L, gwL),(R, gwR)] $ layoutHook def
