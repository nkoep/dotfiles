import Data.List (intercalate)
import Text.Printf (printf)

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS

-- Color definitions
colorHighlight = "#a51f1c"
colorFg = "#fafafa"
colorBg = "#262729"

-- Generic settings
terminal' = "xfce4-terminal"
borderWidth' = 1
workspaces' = ["bla", "blub"]
windowSpacing = 5

-- dmenu customizations
dmenu = intercalate " "
    [ "dmenu_run"
    , "-fn"
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

-- Keybindings
keybindings =
    [ ((modMask', xK_p), spawn dmenu)
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

config' = defaultConfig
    { normalBorderColor = colorBg
    , focusedBorderColor = colorHighlight
    , terminal = terminal'
    , layoutHook
        = gaps (zip [U, D, R, L] $ repeat windowSpacing)
        $ smartSpacing windowSpacing
        $ fullscreenFull . smartBorders . avoidStruts
        $ layoutHook defaultConfig
    , manageHook
        =   manageDocks
        <+> fullscreenManageHook
        -- XXX: This still doesn't fully cut it. For example, VLC doesn't leave
        --      fullscreen immediately after a double-click.
        <+> (isFullscreen --> doFullFloat)
        <+> manageHook defaultConfig
    , handleEventHook = fullscreenEventHook
    , workspaces = workspaces'
    , borderWidth = borderWidth'
    , focusFollowsMouse = False
    , clickJustFocuses = False
    } `additionalKeys` keybindings

main :: IO ()
main = xmonad =<< xmobar config'

