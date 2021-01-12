{-# LANGUAGE OverloadedStrings #-}
module Main where

import Termonad
  ( CursorBlinkMode(CursorBlinkModeOff), Option(Set)
  , ShowScrollbar(ShowScrollbarNever), TMConfig, confirmExit, cursorBlinkMode
  , defaultConfigOptions, defaultTMConfig, options, showMenu, showScrollbar
  , start
  )
import Termonad.Config.Colour
  ( AlphaColour, ColourConfig, Palette(ExtendedPalette), addColourExtension
  , createColour, createColourExtension, cursorBgColour, defaultColourConfig
  , defaultStandardColours, defaultLightColours, foregroundColour, palette
  , List8, mkList8
  )

------------------------------------------
--- Sanity Inc Tomorrow Eighties Theme ---
------------------------------------------

background :: AlphaColour Double
background = createColour 0x2d 0x2d 0x2d

altBackground :: AlphaColour Double
altBackground = createColour 0x33 0x33 0x33

currentLine :: AlphaColour Double
currentLine = createColour 0x39 0x39 0x39

selection :: AlphaColour Double
selection = createColour 0x51 0x51 0x51

foreground :: AlphaColour Double
foreground = createColour 0xcc 0xcc 0xcc

comment :: AlphaColour Double
comment = createColour 0xf2 0x77 0x71

red :: AlphaColour Double
red = createColour 0xf2 0x77 0x7a

orange :: AlphaColour Double
orange = createColour 0xf9 0x91 0x57

yellow :: AlphaColour Double
yellow = createColour 0xff 0xcc 0x66

green :: AlphaColour Double
green = createColour 0x99 0xcc 0x99

aqua :: AlphaColour Double
aqua = createColour 0x66 0xcc 0xcc

blue :: AlphaColour Double
blue = createColour 0x66 0x99 0xcc

purple :: AlphaColour Double
purple = createColour 0xcc 0x99 0xcc

----------------------------------------------
--- Standard Config From Solarized Example ---
----------------------------------------------

myTMConfig :: TMConfig
myTMConfig =
  defaultTMConfig
    { options =
        defaultConfigOptions
          { showScrollbar = ShowScrollbarNever
          , confirmExit = False
          , showMenu = False
          }
    }

myColourConfig :: ColourConfig (AlphaColour Double)
myColourConfig =
  defaultColourConfig
    -- Set the cursor background colour.  This is the normal colour of the
    -- cursor.
    { cursorBgColour = Set yellow
    -- Set the default foreground colour of text of the terminal.
    , foregroundColour = Set foreground
    -- Set the extended palette that has 8 colours standard colors and then 8
    -- light colors.
    , palette = ExtendedPalette (maybe defaultStandardColours id myStandardColours)
                                (maybe defaultLightColours id myLightColours)
    }
  where
    -- This is a an example of creating a length-indexed linked-list of colours,
    -- using 'Vec' constructors.
    myStandardColours :: Maybe (List8 (AlphaColour Double))
    myStandardColours = mkList8
      [ background -- dark brown (used as background colour)
      , red -- red
      , green -- green
      , yellow -- dark yellow
      , purple -- dark purple
      , orange -- bright pink
      , aqua -- teal
      , foreground -- light brown
      ]

    -- This is an example of creating a length-indexed linked-list of colours,
    -- using the 'unsafeFromListVec_' function.  'unsafeFromListVec_' is okay to
    -- use as long as you're absolutely sure you have 8 elements.
    myLightColours :: Maybe (List8 (AlphaColour Double))
    myLightColours = mkList8
        [ createColour  70  60  50 -- brown
        , createColour 220  30  20 -- light red
        , createColour  40 210  20 -- light green
        , createColour 220 200  20 -- yellow
        , createColour  40  30 180 -- purple
        , createColour 140  30 80  -- dark pink
        , createColour  50 200 160 -- light teal
        , createColour 220 200 150 -- light brown
        ]

main :: IO ()
main = do
  -- First, create the colour extension based on 'myColourConfig'.
  myColourExt <- createColourExtension myColourConfig

  -- Update 'myTMConfig' with our colour extension.
  let newTMConfig = addColourExtension myTMConfig myColourExt

  -- Start Termonad with our updated 'TMConfig'.
  start newTMConfig
