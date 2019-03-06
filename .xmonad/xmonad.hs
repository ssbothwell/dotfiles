import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Util.EZConfig
--import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad

import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Hidden
import XMonad.Layout.LayoutCombinators hiding ( (|||) )
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Named
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerScreen
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed
import XMonad.Layout.Gaps
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation

import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects
import XMonad.Actions.Promote
import XMonad.Actions.Navigation2D

import qualified XMonad.StackSet as W
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Unicode

import System.IO
import System.Exit
import qualified Data.Map as M
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Semigroup


-----------------------------------------------------
----------------------- Theme -----------------------
-----------------------------------------------------

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

active       = blue
activeWarn   = red
inactive     = base02
focusColor   = blue
unfocusColor = base02

myFont = "xft:Meslo LG M:style=Regular:size=12"

-- Border Colors
myNormalBorderColor = "#585858"
myFocusedBorderColor = "#d70000"

myTabTheme = def
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = base02
    , activeBorderColor     = active
    , inactiveBorderColor   = base02
    , activeTextColor       = base03
    , inactiveTextColor     = base00
}

topBarTheme = def
    { fontName              = myFont
    , inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
}

-----------------------------------------------------
---------------------- Layouts ----------------------
-----------------------------------------------------

-- sizes
gap    = 4
topbar = 10
border = 0
prompt = 20
status = 20

mySpacing = spacing gap
myGaps = gaps [(U, gap),(D, gap),(L, gap),(R, gap)]


trimNamed :: Int -> String -> l a -> ModifiedLayout Rename l a
trimNamed w n = renamed [CutWordsLeft w, PrependWords n]

suffixed :: String -> l a ->  ModifiedLayout Rename l a
suffixed n = renamed [AppendWords n]

trimSuffixed :: Int -> String -> l a -> ModifiedLayout Rename l a
trimSuffixed w n = renamed [CutWordsRight w, AppendWords n]

myLayoutHook = fullScreenToggle $ flex ||| tabs
    where
        --addTopBar = noFrillsDeco shrinkText topBarTheme
        fullScreenToggle = mkToggle $ single FULL
        nmaster = 1     -- The default number of windows in the master pane
        ratio   = 1/2   -- Default proportion of screen occupied by master pane
        delta   = 3/100 -- Percent of screen to increment by when resizing panes
        smallMonResWidth = 1920

        threeCol = named "ThreeCol" $ ThreeColMid 1 (1/10) (1/2)
        tabs = named "Tabs" $ addTabs shrinkText myTabTheme Simplest
        flex = trimNamed 5 "Flex"
             . avoidStruts
             . windowNavigation
             . addTabs shrinkText myTabTheme
             . subLayout [] (Simplest ||| Accordion)
             $ ifWider smallMonResWidth wideLayouts standardLayouts
             where
                 wideThreeCol = suffixed "Wide 3Col" (ThreeColMid 1 (1/20) (1/2))
                 wideBsp      = trimSuffixed 1 "Wide BSP" (hiddenWindows emptyBSP)
                 wideLayouts  = mySpacing . myGaps $ wideThreeCol ||| wideBsp
                 standardLayouts = mySpacing . myGaps . named "Std 2/3" $ ResizableTall 1 (1/20) (2/3) []

-- Launchers
myBrowser      = "/usr/bin/firefox"
myTerminal     = "/usr/bin/konsole"
myTerminal'    = "/home/solomon/.local/bin/st"
myLauncher     = "rofi -show run"--"exe=`dmenu_path | dmenu` && eval \"exec $exe\""
scriptLauncher = "/home/solomon/.bin/scriptLauncher.py"
myTrello       = "/usr/bin/surf www.trello.com"
myPostman      = "/usr/bin/postman"

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

-----------------------------------------------------
-------------------- Scratchpads --------------------
-----------------------------------------------------

--myScratchPads = [ NS "terminal" spawnTerm  findTerm manageTerm
--                , NS "postman"  spawnPostman findPostman managePostman
--                ]
--
--    where spawnTerm     = myTerminal ++ " -name scratchpad"
--          findTerm      = resource =?   "scratchpad"
--          manageTerm    = customFloating $ W.RationalRect 0.2 0.1 0.6 0.8
--          spawnPostman  = myPostman
--          findPostman   = className =? "postman"
--          managePostman = customFloating $ W.RationalRect 0.2 0.1 0.6 0.8

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.8 -- terminal height, 10%
    w = 0.6 -- terminal width, 60%
    t = 0.1 -- distance from top edge, 10%
    l = 0.2 -- distance from left edge, 40%

-----------------------------------------------------
--------------------- Projects ----------------------
-----------------------------------------------------
-- Work In Progress --
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


-----------------------------------------------------
---------------------- Prompt -----------------------
-----------------------------------------------------

-- A case-insensitive substring predicate function.
predicateFunction :: String -> String -> Bool
predicateFunction x y = lc x `isInfixOf` lc y where lc = map toLower

promptConfig :: XPConfig
promptConfig = def
  { position          = CenteredAt (1/3) (1/2)
  , height            = 30
  , font              = "xft:dejavu sans mono:size=12"
  , bgColor           = "#002b36"
  , fgColor           = "#93a1a1"
  , fgHLight          = "#d33682"
  , bgHLight          = "#073642"
  , borderColor       = red
  , promptBorderWidth = 1
  , maxComplRows      = Just 12
  , alwaysHighlight   = True
  , promptKeymap      = emacsLikeXPKeymap
  , searchPredicate   = predicateFunction
}

projectPrompt     = switchProjectPrompt promptConfig
closeWindowPrompt = confirmPrompt promptConfig "Close Window" kill
closeXmonadPrompt = confirmPrompt promptConfig "Exit XMonad" $ io exitSuccess

-----------------------------------------------------
-------------------- Keybindings --------------------
-----------------------------------------------------

workSpaceNav :: XConfig a -> [(String, X ())]
workSpaceNav c = do
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    (i, j) <- zip (map show [1..9]) $ XMonad.workspaces c
    (m, f) <- [("M-", W.greedyView), ("M-S-", W.shift)]
    return (m++i, windows $ f j)

myKeys c = mkKeymap c $
    ------------------------------
    -- System
    ------------------------------
    [ ("M-q",                    recompile)         -- Recompile Xmonad
    , ("M-S-q",                  closeXmonadPrompt) -- Close Xmonad
    , ("M-<Backspace>",          closeWindowPrompt) -- Close Window
    , ("<XF86AudioMute>",        toggleMute)        -- Mute/Unmute amixer
    , ("<XF86AudioRaiseVolume>", volumeUp)          -- Increase amixer volume
    , ("<XF86AudioLowerVolume>", volumeDown)        -- Decrease amixer volume
    , ("M-i",                    projectPrompt)     -- dynamicProjects prompt
    -- Emoji Insert Prompt :: NOT WORKING
    , ("M-u",                    unicodePrompt "~/.bin/unicode" promptConfig)
    ] ++

    ------------------------------
    -- Navigation
    ------------------------------
    -- Navigate between windows
    [ ("M-j",       windowGo D False)
    , ("M-k",       windowGo U False)
    , ("M-h",       windowGo L False)
    , ("M-l",       windowGo R False)
    -- Navigate between tabs
    , ("M-;",       windows W.focusUp)
    , ("M-'",       windows W.focusDown)
    -- Shift tabs
    , ("M-S-;",     windows W.swapUp)
    , ("M-S-'",     windows W.swapDown)
    -- Swap adjacent windows
    , ("M-S-j",     windowSwap D False)
    , ("M-S-k",     windowSwap U False)
    , ("M-S-h",     windowSwap L False)
    , ("M-S-l",     windowSwap R False)
    -- Shrink/Expand windows
    , ("M-[",       sendMessage Shrink)
    , ("M-]",       sendMessage Expand)
    , ("M-<Space>", sendMessage NextLayout)
    -- Sink floated window
    , ("M-t",       withFocused $ windows . W.sink)
    -- Full Screen a window
    , ("M-<F11>",   sendMessage $ Toggle FULL)
    -- Promote window to master
    , ("M-b",       promote)
    -- "merge with sublayout"
    , ("M-C-h",     sendMessage . pullGroup $ L)
    , ("M-C-l",     sendMessage . pullGroup $ R)
    , ("M-C-j",     sendMessage . pullGroup $ D)
    , ("M-C-k",     sendMessage . pullGroup $ U)
    -- Unmerge a window
    , ("M-g",       withFocused (sendMessage . UnMerge))
    ] ++ workSpaceNav c ++

    ------------------------------
    -- Launchers
    ------------------------------
    [ ("M-<Return>", spawn myTerminal)     -- Launch Terminal
    , ("M-\\",       spawn myBrowser)      -- Launch Browser
    , ("M-p",        spawn myLauncher)     -- Launch DMenu
    , ("M-C-p",      spawn scriptLauncher) -- Script Launcher
    , ("M-o",        spawn myTrello)       -- Launch Trello
    , ("M-`",        terminalScratchpad)   -- Scratchpad Terminal
    ]
    where
        toggleMute         = spawn "amixer -D pulse set Master 1+ toggle"
        volumeUp           = spawn "amixer set Master 10%+"
        volumeDown         = spawn "amixer set Master 10%-"
        recompile          = spawn "xmonad --recompile && xmonad --restart"
        terminalScratchpad = scratchpadSpawnActionTerminal myTerminal
        --terminalScratchpad = namedScratchpadAction myScratchPads "terminal"
        --postmanScratchpad  = namedScratchpadAction myScratchPads "postman"

myNav2DConf = def
    { defaultTiledNavigation = centerNavigation
    , floatNavigation        = centerNavigation
    , screenNavigation       = lineNavigation
    , layoutNavigation       = pure ("Full", centerNavigation)
    , unmappedWindowRect     = pure ("Full", singleWindowRect)
    }

-- Mouse Bindings
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                      >> windows W.shiftMaster) -- Set window to float and move by dragging
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster) -- Raise the window to the top of the stack
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                      >> windows W.shiftMaster) -- Set window to float and resize by dragging
    ]

-----------------------------------------------------
------------------------ Main -----------------------
-----------------------------------------------------

main = do
    xmproc <- spawnPipe "~/.local/bin/xmobar ~/.xmobarrc"
    xmonad . docks . dynamicProjects projects . withNavigation2DConfig myNav2DConf $ def
        { layoutHook            = avoidStruts myLayoutHook
        , manageHook            = myManageHook <> manageHook def <> manageScratchPad
        , logHook               = dynamicLogWithPP xmobarPP
            { ppOutput          = hPutStrLn xmproc
            --, ppLayout          = drop 18
            , ppTitle           = xmobarColor "green" "" . shorten 150
            , ppHidden          = \ws -> if ws == "NSP" then "" else ws
            , ppHiddenNoWindows = const mempty
            }
        , modMask               = mod4Mask
        , keys                  = myKeys
        --, myMouseBindings       = myMouseBindings
        , workspaces            = myWorkspaces
        , normalBorderColor     = myNormalBorderColor
        , focusedBorderColor    = myFocusedBorderColor
        }
