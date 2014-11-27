import Data.List (intercalate, elemIndex)
import Text.Printf (printf)
import System.IO (hPutStrLn)
import Control.Monad (liftM2)

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers (isFullscreen, doCenterFloat, doFullFloat)
import qualified XMonad.Hooks.EwmhDesktops as E
import XMonad.Layout.Renamed (renamed, Rename (Replace))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Fullscreen ( fullscreenFull
                                , fullscreenEventHook
                                , fullscreenManageHook
                                )
import XMonad.Layout.Gaps (gaps)
import XMonad.Layout.Spacing (smartSpacing)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (safeSpawn, spawnPipe)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowBringer (gotoMenuArgs)
-- This module is part of the xmonad-extras package.
import XMonad.Actions.Volume (raiseVolume, lowerVolume)
import qualified XMonad.StackSet as W

-- Color definitions
colorHighlight = "#a51f1c"
colorFg = "#fafafa"
colorBg = "#262729"

-- Generic settings
terminal' = "xfce4-terminal"
borderWidth' = 1
-- TODO: Define variables for these so we don't have to hard-code workspace
--       names below.
workspaces' = ["bla", "web", "mail", "media", "irc", "misc"]
windowSpacing = 5

-- Layouts
-- TODO: Use XMonad.Layout.PerWorkspace.onWorkspace to use full layout per
--       default on the mail workspace.
layouts = tiled ||| mtiled ||| full
    where goldenRatio       = (1+(toRational(sqrt(5)::Double))) / 2
          renameLayout name = renamed [Replace name]
          tiled             = renameLayout "T" $ Tall 1 (1/100) (1/goldenRatio)
          mtiled            = renameLayout "MT" $ Mirror tiled
          full              = renameLayout "F" $ Full

-- Explicit window management hooks
manageHooks = composeAll
    [ -- Floated windows
      className =? "Gimp" --> doFloat
      -- Center-floated windows
    , className =? "Gmrun" --> doCenterFloat
    , className =? "ioquake3" --> doCenterFloat
    , className =? "Volumeicon" --> doCenterFloat
    , className =? "Settings" --> doCenterFloat
      -- Windows with default workspaces
    , className =? "Thunderbird" --> doShift "mail"
    , className =? "Blaplay" --> viewShift "media"
    , isFullscreen --> doFullFloat
    ]
    where viewShift = doF . liftM2 (.) W.greedyView W.shift

-- Pretty-printer for xmobar
prettyPrinter handle = defaultPP
    { ppCurrent = xmobarColor colorHighlight "" . formatWSString
    , ppVisible = wrap "[" "]" . formatWSString
    , ppHidden = formatWSString
    , ppHiddenNoWindows = (++ "*") . formatWSString
    , ppTitle = shorten 50
    -- FIXME: Remove the "SmartSpacing 5" string added when using the
    --        smartSpacing layout modifier.
    , ppLayout = id
    , ppOutput = hPutStrLn handle
    }
    -- TODO: Add a star (*) to workspaces that have windows.
    where formatWSString workspaceId =
            case (workspaceId `elemIndex` workspaces') of
                Just idx -> let idx' = show (idx+1)
                            in addAction idx' $ idx' ++ ":" ++ workspaceId
                Nothing  -> workspaceId
          addAction idx =
            wrap ("<action=`xdotool key alt+" ++ idx ++ "`>") "</action>"

-- dmenu customizations (requires the `dmenu-xft-height` AUR package)
dmenuOptions =
    [ "-b"
    , "-fn"
    , "'DejaVu Sans-8'"
    , "-nb"
    , colorBg
    , "-nf"
    , colorFg
    , "-sb"
    , colorHighlight
    , "-h"
    , "30"
    ]

-- Keybindings
keybindings =
    [ ((modMask', xK_p), safeSpawn "dmenu_run" dmenuOptions)
    , ((modMask', xK_n), spawn "nemo")
    , ((smMask, xK_f), gotoMenuArgs dmenuOptions)
    , ((modMask', xK_F5), spawn "slock")
    , ((cmMask, xK_h), prevWS)
    , ((cmMask, xK_l), nextWS)
    , ((scmMask, xK_h), shiftToPrev)
    , ((scmMask, xK_l), shiftToNext)
    -- TODO: Add bindings for `prevScreen` and `nextScreen`.
    -- TODO: Add bindings for media keys, e.g., XF86MonBrightnessUp ->
    --       xbacklight +10 (use `parseKey`).
    , ((0, xK_F11), raiseVolume' stepSize)
    , ((0, xK_F12), lowerVolume' stepSize)
    , ((modMask', xK_q), safeSpawn "xmonad" ["--replace"])
    ]
    where modMask' = mod1Mask
          smMask   = shiftMask .|. modMask'
          cmMask   = controlMask .|. modMask'
          scmMask  = shiftMask .|. cmMask
          raiseVolume' x = raiseVolume x >> return ()
          lowerVolume' x = lowerVolume x >> return ()
          stepSize = 2

config' handle = E.ewmh defaultConfig
    { normalBorderColor = colorFg
    , focusedBorderColor = colorHighlight
    , terminal = terminal'
    , layoutHook
        = avoidStruts
        -- TODO: Implement smartGaps which should work similar to smartSpacing,
        --       i.e., only display the gaps if there's more than one window
        --       in a workspace.
        $ gaps (zip [U, D, R, L] $ repeat windowSpacing)
        -- TODO: Override smartSpacing resp. the modifierDescription of
        --       SmartSpacing to get rid of the the "SmartSpacing prefix" in
        --       layout names.
        $ smartSpacing windowSpacing
        $ smartBorders
        $ layouts
    , manageHook
        =   manageDocks
        <+> manageHooks
        <+> fullscreenManageHook
    , handleEventHook
        =   docksEventHook
        -- TODO: Test if we can drop fullscreenEventHook in favor of the EWMH
        --       variant. Without the latter, we can convince VLC to go into
        --       fullscreen but not smplayer.
        <+> fullscreenEventHook
        <+> E.fullscreenEventHook
    , workspaces = workspaces'
    , borderWidth = borderWidth'
    , logHook = dynamicLogWithPP $ prettyPrinter handle
    , startupHook = setDefaultCursor xC_left_ptr
    , focusFollowsMouse = False
    , clickJustFocuses = False
    } `additionalKeys` keybindings

-- xmobar configuration
main :: IO ()
main = do
    handle <- spawnPipe "~/.cabal/bin/xmobar ~/.config/xmobar/xmobarrc_top"
    xmonad $ config' handle

