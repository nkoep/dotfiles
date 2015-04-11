import Data.List (intercalate, elemIndex)
import Text.Printf (printf)
import System.IO (hPutStrLn)

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers (isFullscreen, doCenterFloat, doFullFloat)
import qualified XMonad.Hooks.EwmhDesktops as E
import XMonad.Layout.Renamed (renamed, Rename (Replace))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Fullscreen
    ( fullscreenFull
    , fullscreenEventHook
    , fullscreenManageHook
    )
import XMonad.Layout.Gaps (gaps)
import XMonad.Layout.Spacing (spacing)
import XMonad.Util.EZConfig (additionalKeys)
import Graphics.X11.ExtraTypes.XF86
    ( xF86XK_MonBrightnessUp
    , xF86XK_MonBrightnessDown
    )
import XMonad.Util.Run (safeSpawn, spawnPipe)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowBringer (gotoMenuArgs)

-- Color definitions
colorHighlight = "#a51f1c"
colorFg = "#fafafa"
colorBg = "#262729"

-- Generic settings
terminal' = "xfce4-terminal"
borderWidth' = 1
-- TODO: Migrate to on-demand workspaces.
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
    , className =? "ioquake3" --> doCenterFloat
    , className =? "Volumeicon" --> doCenterFloat
    , className =? "Settings" --> doCenterFloat
    , isFullscreen --> doFullFloat
    ]

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
    , ((modMask', xK_q), safeSpawn "xmonad" ["--replace"])
    , ((mod4Mask, xF86XK_MonBrightnessUp), raiseBrightness)
    , ((mod4Mask, xF86XK_MonBrightnessDown), lowerBrightness)
    ]
    where modMask'           = mod1Mask
          smMask             = shiftMask .|. modMask'
          cmMask             = controlMask .|. modMask'
          scmMask            = shiftMask .|. cmMask
          brightnessStep     = "5%"
          adjustBrightness o = safeSpawn "xbacklight" [o : brightnessStep]
          raiseBrightness    = adjustBrightness '+'
          lowerBrightness    = adjustBrightness '-'

startupHook' = do
    setDefaultCursor xC_left_ptr
    spawn "xprofile auto"
    spawn "~/Dropbox/bla/.bin/trayer-start"
    spawn "systemctl --user restart dropbox"
    spawn "xbacklight -set 80%"

config' handle = E.ewmh defaultConfig
    { normalBorderColor = colorFg
    , focusedBorderColor = colorHighlight
    , terminal = terminal'
    , layoutHook
        = avoidStruts
        $ gaps (zip [U, D, R, L] $ repeat windowSpacing)
        $ spacing windowSpacing
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
    , startupHook = startupHook'
    , focusFollowsMouse = False
    , clickJustFocuses = False
    } `additionalKeys` keybindings

-- xmobar configuration
main :: IO ()
main = do
    handle <- spawnPipe "xmobar ~/.config/xmobar/xmobarrc"
    xmonad $ config' handle

