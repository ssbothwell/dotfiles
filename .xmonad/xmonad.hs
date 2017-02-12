import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import System.IO

-- Border Colors
myNormalBorderColor = "#585858"
myFocusedBorderColor = "#d70000"


-- Gap Widths
gapwidth = 4
gwU = 8
gwD = 8
gwL = 8
gwR = 8


-- Workspaces
myWorkspaces = ["1:term","2:web", "3:files", "4:dev", "5:steam"] ++ map show [4..9]

-- Window Rules
myManageHook = composeAll
        [ className =? "Chromium"       --> doShift "2:web"
        , className =? "Steam"          --> doShift "5:steam"
        , className =? "stalonetray"    --> doIgnore
        , className =? "thunar"         --> doShift "3:files"
        , manageDocks  
        ]
main = do
        xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
        xmonad $ defaultConfig
                { layoutHook = avoidStruts $ myLayout
                , manageHook = manageHook defaultConfig <+> myManageHook 
                , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 150
                        }
                , modMask = mod4Mask
                , workspaces = myWorkspaces
                , normalBorderColor = myNormalBorderColor
                , focusedBorderColor = myFocusedBorderColor
                } `additionalKeys`
                [ ((mod4Mask, xK_z), spawn "yeganesh")
                , ((mod4Mask, xK_F11), spawn "amixer -D pulse sset Master 10%-") 
                , ((mod4Mask, xK_F12), spawn "amixer -D pulse sset Master 10%+")
                , ((0, xK_Print), spawn "scrot")
                , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
                ]


myLayout = spacing gapwidth $ gaps [(U, gwU),(D, gwD),(L, gwL),(R, gwR)] $ layoutHook defaultConfig
