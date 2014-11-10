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
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Actions.CycleWS
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
workspaces' = ["bla", "mail", "media", "irc", "misc"]
windowSpacing = 5

-- Layouts
layouts = tiled ||| mtiled ||| full
    where goldenRatio       = (2 / (1+(toRational(sqrt(5)::Double))))
          renameLayout name = renamed [Replace name]
          tiled             = renameLayout "T" $ Tall 1 (1/100) goldenRatio
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
      -- Windows with default workspaces
    , className =? "Thunderbird" --> doShift "mail"
    , className =? "Blaplay" --> viewShift "media"
    ]
    where viewShift = doF . liftM2 (.) greedyView shift

-- Pretty-printer for xmobar
prettyPrinter handle = defaultPP
    { ppCurrent = xmobarColor colorHighlight "" . prependWSIndex
    , ppVisible = wrap "[" "]" . prependWSIndex
    , ppHidden = prependWSIndex
    , ppHiddenNoWindows = prependWSIndex
    -- FIXME: Remove the "SmartSpacing 5" string added when using the
    --        smartSpacing layout modifier.
    -- , ppLayout = const "bla"
    , ppOutput = hPutStrLn handle
    }
    where prependWSIndex workspaceId =
            case (workspaceId `elemIndex` workspaces') of
                Just idx -> show (idx+1) ++ ":" ++ workspaceId
                Nothing  -> workspaceId

-- dmenu customizations
dmenuOptions =
    [ "-fn"
    , "'DejaVu Sans-8'"
    , "-nb"
    , quoteString colorBg
    , "-nf"
    , quoteString colorFg
    , "-sb"
    , quoteString colorHighlight
    , "-h"
    , "30"
    ]
    where quoteString s = printf "'%s'" s
dmenu = intercalate " " $ "dmenu_run" : dmenuOptions

-- Keybindings
keybindings =
    [ ((modMask', xK_p), spawn dmenu)
    , ((modMask', xK_n), spawn "nemo")
    , ((cmMask, xK_h), prevWS)
    , ((cmMask, xK_l), nextWS)
    , ((scmMask, xK_h), shiftToPrev)
    , ((scmMask, xK_l), shiftToNext)
    -- TODO: Add bindings for `prevScreen` and `nextScreen`.
    -- TODO: Add bindings for media keys (use `parseKey`).
    ]
    where modMask' = mod1Mask
          cmMask = controlMask .|. modMask'
          scmMask = shiftMask .|. cmMask

config' handle = defaultConfig
    { normalBorderColor = colorBg
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
    , startupHook = setDefaultCursor xC_left_ptr
    , focusFollowsMouse = False
    , clickJustFocuses = False
    } `additionalKeys` keybindings

-- xmobar configuration
main :: IO ()
main = do
    handle <- spawnPipe "~/.cabal/bin/xmobar"
    xmonad $ config' handle

