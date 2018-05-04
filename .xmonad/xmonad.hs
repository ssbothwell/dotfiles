import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Actions.DynamicProjects
import XMonad.Prompt
import XMonad.Layout.Named
import System.IO
import System.Exit
import qualified Data.Map               as M
import qualified XMonad.StackSet        as W
import Data.Char (toLower)
import Data.List (isInfixOf)


-- Border Colors
myNormalBorderColor = "#585858"
myFocusedBorderColor = "#d70000"

-- Gap Widths
gapwidth = 4
gwU = 8
gwD = 8
gwL = 8
gwR = 8

--Gaps
myLayoutHook = spacing gapwidth $ gaps [(U, gwU),(D, gwD),(L, gwL),(R, gwR)] $ myLayouts

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

-- Launchers
myBrowser  = "/usr/bin/firefox"
myTerminal = "/home/solomon/.local/bin/st"      
myLauncher = "rofi -show run"--"exe=`dmenu_path | dmenu` && eval \"exec $exe\""
myTrello   = "/usr/bin/surf www.trello.com"
myPostman  = "/usr/bin/postman"

-- Workspaces
myWorkspaces = ["1:term","2:web", "3:slack", "4:ranger", "5:trello", "6:sys"] ++ map show [7..9]

-- Window Rules
myManageHook = composeAll
    [ className =? "Wicd-client.py" --> doFloat
    , className =? "postman"        --> doFloat
    , className =? "Firefox"        --> doShift "2:web"
    , className =? "Slack"          --> doShift "3:slack"
    , className =? "stalonetray"    --> doIgnore
    , manageDocks  
    ]

-- Scratchpads

myScratchPads = [ NS "terminal" spawnTerm  findTerm manageTerm
                , NS "postman"  spawnPostman findPostman managePostman
                ]

    where spawnTerm     = myTerminal ++ " -name scratchpad"
          findTerm      = resource =?   "scratchpad"
          manageTerm    = customFloating $ W.RationalRect 0.2 0.1 0.6 0.8
          spawnPostman  = myPostman
          findPostman   = className =? "postman"
          managePostman = customFloating $ W.RationalRect 0.2 0.1 0.6 0.8
          
--manageScratchPad :: ManageHook
--manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
--  where
--    h = 0.8     -- terminal height, 10%
--    w = 0.6     -- terminal width, 60%
--    t = 0.1     -- distance from top edge, 10%
--    l = 0.2     -- distance from left edge, 40%

-- Projects
projects :: [Project]
projects =
    [ Project { projectName      = "haskell-book"
              , projectDirectory = "~/Development/haskell/haskell_book"
              , projectStartHook = Just $ do spawn "urxvt"
                                             spawn "urxvt"
              }
    , Project { projectName      = "Tripp Inc"
              , projectDirectory = "~/Development/trippinc/firebase-backend/src"
              , projectStartHook = Just $ do spawn "urxvt"
                                             spawn "urxvt"
              }

    ]

-- A case-insensitive substring predicate function.
predicateFunction :: String -> String -> Bool
predicateFunction x y = lc x `isInfixOf` lc y where lc = map toLower

promptConfig :: XPConfig
promptConfig = def
  { position          = CenteredAt (1/3) (1/2)
  , height            = 35
  , font              = "xft:dejavu sans mono:size=14"
  , bgColor           = "#002b36"
  , fgColor           = "#93a1a1"
  , fgHLight          = "#d33682"
  , bgHLight          = "#073642"
  , borderColor       = "#053542"
  , promptBorderWidth = 5
  , maxComplRows      = Just 12
  , alwaysHighlight   = True
  , promptKeymap      = emacsLikeXPKeymap
  , searchPredicate   = predicateFunction
}
------------------------------------------------------------

-- Keybindings
myKeys = \c -> mkKeymap c $ 

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [ (m ++ i, windows $ f j)
        | (i, j) <- zip (map show [1..9]) (XMonad.workspaces c)
        , (m, f) <- [("M-", W.greedyView), ("M-S-", W.shift)] --Shift wndw to ws
    ]
    ++
    [ (("M-<Return>")             , spawn myTerminal)               -- Launch Terminal
    , (("M-\\")                   , spawn myBrowser)                -- Launch Browser
    , (("M-p")                    , spawn myLauncher)               -- Launch DMenu
    , (("M-o")                    , spawn myTrello)                 -- Launch Trello
    , (("M-<Backspace>")          , kill)                           -- Close focused window.
    , (("M-`")                    , terminalScratchpad)             -- Scratchpad Terminal
    , (("M-r")                    , postmanScratchpad)              -- Scratchpad Postman
    , (("M-i")                    , projectPrompt)                  -- dynamicProjects prompt
    , (("<XF86AudioMute>")        , toggleMute)                     -- Mute/Unmute amixer
    , (("<XF86AudioRaiseVolume>") , volumeUp)                       -- Increase amixer volume
    , (("<XF86AudioLowerVolume>") , volumeDown)                     -- Decrease amixer volume
    , (("M-q")                    , recompile)                      -- Restart Xmonad
    , (("M-S-q")                  , io (exitWith ExitSuccess))      -- Quit xmonad
    , (("M-j")                    , windows W.focusDown)            -- Move focus to the next window.
    , (("M-k")                    , windows W.focusUp)              -- Move focus to the previous window.
    , (("M-S-j")                  , windows W.swapDown)             -- Swap the focused window with the next window.
    , (("M-S-k")                  , windows W.swapUp)               -- Swap the focused window with the previous window.
    , (("M-h")                    , sendMessage Shrink)             -- Shrink the master area.
    , (("M-l")                    , sendMessage Expand)             -- Expand the master area.
    , (("M-<Space>")              , sendMessage NextLayout)         -- Cycle through the available layout algorithms.
    , (("M-t")                    , withFocused $ windows . W.sink) -- Push window back into tiling
    ]
      where 
            toggleMute         = spawn "amixer -D pulse set Master 1+ toggle"
            volumeUp           = spawn "amixer set Master 10%+"
            volumeDown         = spawn "amixer set Master 10%-"
            recompile          = spawn "xmonad --recompile && xmonad --restart"
            terminalScratchpad = namedScratchpadAction myScratchPads "terminal"
            postmanScratchpad  = namedScratchpadAction myScratchPads "postman"
            projectPrompt      = switchProjectPrompt promptConfig

-- Mouse Bindings
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w 
                                       >> windows W.shiftMaster)) -- Set window to float and move by dragging
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster)) -- Raise the window to the top of the stack
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w 
                                       >> windows W.shiftMaster)) -- Set window to float and resize by dragging
    ]
 

main = do
    xmproc <- spawnPipe "~/.local/bin/xmobar ~/.xmobarrc"
    xmonad $ docks $ dynamicProjects projects def
        { layoutHook            = avoidStruts $ myLayoutHook
        , manageHook            = myManageHook <+> manageHook def <+> namedScratchpadManageHook myScratchPads   
        , logHook               = dynamicLogWithPP xmobarPP
            { ppOutput          = hPutStrLn xmproc
            , ppLayout          = (\x -> drop 10 x)
            , ppTitle           = xmobarColor "green" "" . shorten 150
            , ppHidden          = noScratchPad
            , ppHiddenNoWindows = (\x -> "")
            }
        , modMask               = mod4Mask
        , keys                  = myKeys
        --, myMouseBindings       = myMouseBindings
        , workspaces            = myWorkspaces
        , normalBorderColor     = myNormalBorderColor
        , focusedBorderColor    = myFocusedBorderColor
        }
        where noScratchPad ws = if ws == "NSP" then "" else ws

