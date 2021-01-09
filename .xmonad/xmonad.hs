import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)

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
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation

import XMonad.Actions.CopyWindow
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
import Data.Maybe (maybeToList)
import Data.Function (on)

-------------
--- Theme ---
-------------

background = "#2d2d2d"
altBackground = "#333333"
currentLine = "#393939"
selection = "#515151"
foreground = "#cccccc"
comment = "#999999"

red    = "#f2777a"
orange = "#f99157"
yellow = "#ffcc66"
green  = "#99cc99"
aqua   = "#66cccc"
blue   = "#6699cc"
purple = "#cc99cc"

active       = red
activeWarn   = blue
inactive     = orange
focusColor   = red
unfocusColor = orange

myFont = "xft:Meslo LG M:style=Regular:size=12"

myNormalBorderColor = blue
myFocusedBorderColor = red

myTabTheme = def
    { fontName              = myFont
    , activeColor           = altBackground
    , inactiveColor         = background
    , activeBorderColor     = altBackground
    , inactiveBorderColor   = background
    , activeTextColor       = foreground
    , inactiveTextColor     = comment
}

---------------
--- Layouts ---
---------------

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
        fullScreenToggle = mkToggle $ single FULL
        nmaster = 1     -- The default number of windows in the master pane
        ratio   = 1/2   -- Default proportion of screen occupied by master pane
        delta   = 3/100 -- Percent of screen to increment by when resizing panes
        smallMonResWidth = 1920

        threeCol = named "ThreeCol" $ ThreeColMid 1 (1/10) (1/2)
        tabs = named "Tabs" $ addTabs shrinkText myTabTheme Simplest
        flex = trimNamed 5 "Flex"
             . windowNavigation
             . addTabs shrinkText myTabTheme
             . subLayout [] (Simplest ||| simpleFloat)
             $ standardLayouts
             -- $ ifWider smallMonResWidth wideLayouts standardLayouts
             where
                 wideThreeCol = suffixed "Wide 3Col" (ThreeColMid 1 (1/20) (1/2))
                 wideLayouts  = mySpacing . myGaps $ wideThreeCol
                 standardLayouts = mySpacing . myGaps . named "Std 2/3" $ ResizableTall 1 (1/20) (2/3) []

myBrowser      = "/usr/bin/firefox"
myTerminal     = "termonad"
myLauncher     = mconcat $ intersperse " " [path, font, bgcolor, fgcolor, sfcolor, sbcolor]
    where
        path = "dmenu_run"
        font = "-fn \"" <> myFont <> "\""
        bgcolor = "-nb " <> show background
        fgcolor = "-nf " <> show orange
        sbcolor = "-sb " <> show background
        sfcolor = "-sf " <> show purple

myWorkspaces = ["1:term","2:web", "3:slack"] ++ map show [4..9]


myManageHook = composeAll
    [ className =? "Firefox" --> doShift "2:web"
    , className =? "Slack"   --> doShift "3:slack"
    , className =? "trayer"  --> doIgnore
    , manageDocks
    ]

--------------
--- Prompt ---
--------------

promptConfig :: XPConfig
promptConfig = def
  { position          = Top
  , height            = 20
  , font              = myFont
  , bgColor           = background
  , fgColor           = orange
  , fgHLight          = "#d33682"
  , bgHLight          = "#073642"
  , promptBorderWidth = 0
  , maxComplRows      = Just 12
  , alwaysHighlight   = True
  , promptKeymap      = emacsLikeXPKeymap
  , searchPredicate   = isInfixOf `on` (map toLower)
}

closeWindowPrompt = confirmPrompt promptConfig "Close Window" kill1
closeXmonadPrompt = confirmPrompt promptConfig "Exit XMonad" $ io exitSuccess

-------------------
--- Keybindings ---
-------------------

workSpaceNav :: XConfig a -> [(String, X ())]
workSpaceNav c = do
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
    ] <>

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
    , ("M-C-<Space>", toSubl NextLayout)
    -- Float/Sink floated window
    , ("M-t",       withFocused toggleFloat >> killAllOtherCopies)
    , ("M-C-t",     withFocused toggleSticky)
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
    ] <> workSpaceNav c <>
    [ ("M-<Return>", spawn myTerminal)     -- Launch Terminal
    , ("M-\\",       spawn myBrowser)      -- Launch Browser
    , ("M-p",        spawn myLauncher)     -- Launch DMenu
    ]
    where
        toggleMute     = spawn "amixer -D pulse set Master 1+ toggle"
        volumeUp       = spawn "amixer set Master 5%+"
        volumeDown     = spawn "amixer set Master 5%-"
        recompile      = spawn "xmonad --recompile && xmonad --restart"
        toggleSticky w = windows $ \s ->
            if M.member w (W.floating s)
            then copyToAll s
            else s
        toggleFloat w  = windows $ \s ->
            if M.member w (W.floating s)
            then W.sink w s
            else W.float w (W.RationalRect (1/6) (1/6) (2/3) (2/3)) s
myNav2DConf = def
    { defaultTiledNavigation = centerNavigation
    , floatNavigation        = centerNavigation
    , screenNavigation       = lineNavigation
    , layoutNavigation       = pure ("Full", centerNavigation)
    , unmappedWindowRect     = pure ("Full", singleWindowRect)
    }

myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster) -- Set window to float and move by dragging
    , ((modm .|. controlMask, button1), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster) -- Set window to float and resize by dragging
    ]

------------
--- Main ---
------------

myStartupHook :: X ()
myStartupHook = do
    spawn "nm-applet"
    spawn "feh --bg-scale /home/solomon/Images/Wallpapers/Vaporwave.jpg"
    spawn "xbanish"
    spawn "trayer --edge top --width 4 --align right --height 23 --transparent true --alpha 75 --tint 0x2d2d2d"
    spawn "dunst"
    spawn "sleep 2 && kmonad .local/etc/kmonad.kbd"


myConfig xmproc = def
    { layoutHook            = myLayoutHook
    , manageHook            = myManageHook <> manageHook def
    , logHook               = dynamicLogWithPP xmobarPP
        { ppCurrent         = xmobarColor yellow mempty
        , ppOutput          = hPutStrLn xmproc
        , ppLayout          = drop 18
        , ppTitle           = xmobarColor foreground mempty . shorten 85
        , ppHidden          = \ws -> if ws == "NSP" then mempty else ws
        , ppHiddenNoWindows = const mempty
        }
    , modMask               = mod4Mask
    , keys                  = myKeys
    , mouseBindings       = myMouseBindings
    , workspaces            = myWorkspaces
    , normalBorderColor     = myNormalBorderColor
    , focusedBorderColor    = myFocusedBorderColor
    , startupHook           = myStartupHook
    }

addSupported :: [String] -> X ()
addSupported props = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    -- fs <- getAtom "_NET_WM_STATE_FULLSCREEN"
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
