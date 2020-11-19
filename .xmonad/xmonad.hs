import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)

import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Gaps
import XMonad.Layout.Hidden
import XMonad.Layout.LayoutCombinators hiding ((|||))
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerScreen
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation

import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects
import XMonad.Actions.Navigation2D
import XMonad.Actions.Promote

import qualified XMonad.StackSet as W
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt

import System.IO
import System.Exit

import Control.Monad

import qualified Data.Map as M
import Data.Char (toLower)
import Data.List (isInfixOf, intersperse, nub)
import Data.Semigroup
import Data.Maybe (maybeToList)


-----------------------------------------------------
----------------------- Theme -----------------------
-----------------------------------------------------

background = "#2d2d2d"
altBackground = "#333333"
currentLine = "393939"
selection = "#515151"
foreground = "#cccccc"
comment = "#f27771"


base03  = "#002b36"
base02  = "#073642"
base00  = "#657b83"
base0   = "#839496"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

active       = red
activeWarn   = blue
inactive     = base02
focusColor   = red
unfocusColor = base02

myFont = "xft:Meslo LG M:style=Regular:size=12"

-- Border Colors
myNormalBorderColor = "#585858"
myFocusedBorderColor = "#d70000"

myTabTheme = def
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = base02
    , activeBorderColor     = green
    , inactiveBorderColor   = background
    , activeTextColor       = foreground
    , inactiveTextColor     = comment
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

myLayoutHook = avoidStruts $ fullScreenToggle $ flex ||| tabs
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
             -- . avoidStruts
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
myTerminal     = "/home/solomon/.local/bin/termonad"
scriptLauncher = "/home/solomon/.scripts/scriptLauncher.py"
myTrello       = "/usr/bin/surf www.trello.com"
mySpotify      = "/usr/bin/spotify"
myLauncher     = mconcat $ intersperse " " [path, font, bgcolor, fgcolor, sfcolor, sbcolor]
    where
        path = "dmenu_run"
        font = "-fn \"xft:Bitstream Vera Sans Mono:size=11:bold:antialias=true\""
        bgcolor = "-nb " <> show base03
        fgcolor = "-nf " <> show green
        sbcolor = "-sb " <> show base02
        sfcolor = "-sf " <> show base0
-- Workspaces
myWorkspaces = ["1:term","2:web", "3:slack"] ++ map show [4..9]

-- Window Rules
myManageHook = composeAll
    [ className =? "Firefox"     --> doShift "2:web"
    , className =? "Slack"       --> doShift "3:slack"
    , className =? "stalonetray" --> doIgnore
    , manageDocks
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
    ]
    where
        toggleMute         = spawn "amixer -D pulse set Master 1+ toggle"
        volumeUp           = spawn "amixer set Master 5%+"
        volumeDown         = spawn "amixer set Master 5%-"
        recompile          = spawn "xmonad --recompile && xmonad --restart"

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

myStartupHook :: X ()
myStartupHook = do
    spawn "nm-applet"
    spawn "feh --bg-scale /home/solomon/Images/Wallpapers/Vaporwave.jpg"
    spawn "xbanish"
    spawn "trayer --edge top --width 4 --align right --height 23 --transparent true --alpha 75 --tint 0x2d2d2d"
    spawn "sleep 2 && kmonad .local/etc/kmonad.kbd"


myConfig xmproc = def
    { layoutHook            = myLayoutHook
    , manageHook            = myManageHook <> manageHook def
    , logHook               = dynamicLogWithPP xmobarPP
        { ppOutput          = hPutStrLn xmproc
        , ppLayout          = drop 18
        , ppTitle           = xmobarColor "green" "" . shorten 250
        , ppHidden          = \ws -> if ws == "NSP" then "" else ws
        , ppHiddenNoWindows = const mempty
        }
    , modMask               = mod4Mask
    , keys                  = myKeys
    --, myMouseBindings       = myMouseBindings
    , workspaces            = myWorkspaces
    , normalBorderColor     = myNormalBorderColor
    , focusedBorderColor    = myFocusedBorderColor
    , startupHook           = myStartupHook
    }

addSupported :: [String] -> X ()
addSupported props = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    fs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    newSupportedList <- mapM (fmap fromIntegral . getAtom) props
    io $ do
        supportedList <- fmap (join . maybeToList) $ getWindowProperty32 dpy a r
        changeProperty32 dpy r a aTOM propModeReplace (nub $ newSupportedList ++ supportedList)

setFullscreenSupported :: X ()
setFullscreenSupported = addSupported ["_NET_WM_STATE", "_NET_WM_STATE_FULLSCREEN"]

ewmhFullscreen :: XConfig a -> XConfig a
ewmhFullscreen c = c { startupHook     = startupHook c <+> setFullscreenSupported
                     , handleEventHook = handleEventHook c <+> fullscreenEventHook }

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar ~/.xmobarrc"
    xmonad . ewmhFullscreen . ewmh . docks . withNavigation2DConfig myNav2DConf $ myConfig xmproc
