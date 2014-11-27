import Data.List (intercalate, elemIndex)
import Text.Printf (printf)
import System.IO (hPutStrLn)
import Control.Monad (liftM2)

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers (isFullscreen, doCenterFloat, doFullFloat)
import XMonad.Layout.Renamed (renamed, Rename (Replace))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps (gaps)
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (safeSpawn, spawnPipe)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowBringer (gotoMenuArgs)
-- This module is part of the xmonad-extras package.
import XMonad.Actions.Volume (raiseVolume, lowerVolume)
import XMonad.StackSet (greedyView, shift)

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
    , className =? "Vlc" --> doCenterFloat -- TODO: Push to `media` WS.
    , className =? "ioquake3" --> doCenterFloat -- TODO: Push to `misc` WS.
    , className =? "Volumeicon" --> doCenterFloat
    , className =? "Settings" --> doCenterFloat
      -- Windows with default workspaces
    , className =? "Thunderbird" --> doShift "mail"
    , className =? "Blaplay" --> viewShift "media"
    ]
    where viewShift = doF . liftM2 (.) greedyView shift

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
    ]
    where modMask' = mod1Mask
          smMask   = shiftMask .|. modMask'
          cmMask   = controlMask .|. modMask'
          scmMask  = shiftMask .|. cmMask
          raiseVolume' x = raiseVolume x >> return ()
          lowerVolume' x = lowerVolume x >> return ()
          stepSize = 2

startupHook' = do
    setDefaultCursor xC_left_ptr
    spawn "~/Dropbox/bla/.bin/configure-session"

config' handle = defaultConfig
    { normalBorderColor = colorFg
    , focusedBorderColor = colorHighlight
    , terminal = terminal'
    , layoutHook
        -- FIXME: gaps and smartSpacing handle space differently.
        = gaps (zip [U, D] $ repeat (2 * windowSpacing))
        $ smartSpacing windowSpacing
        $ avoidStruts . fullscreenFull . smartBorders
        $ layouts
    , manageHook
        =   manageDocks
        <+> manageHooks
        <+> fullscreenManageHook
        -- XXX: This still doesn't fully cut it. For example, VLC doesn't leave
        --      fullscreen immediately after a double-click.
        <+> (isFullscreen --> doFullFloat)
        <+> manageHook defaultConfig
    , handleEventHook = fullscreenEventHook
    , workspaces = workspaces'
    , borderWidth = borderWidth'
    , logHook = dynamicLogWithPP $ prettyPrinter handle
    , startupHook = startupHook'
    , focusFollowsMouse = False
    , clickJustFocuses = False
    } `additionalKeys` keybindings

-- xmobar configuration
main :: IO ()
main = do
    handle <- spawnPipe "~/.cabal/bin/xmobar ~/.config/xmobar/xmobarrc_top"
    xmonad $ config' handle

