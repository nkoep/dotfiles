import Data.List (intercalate, elemIndex)
import Text.Printf (printf)
import System.IO (hPutStrLn)

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import qualified XMonad.Hooks.EwmhDesktops as E
import XMonad.Layout.Renamed (renamed, Rename (Replace))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Fullscreen (fullscreenManageHook)
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
focusBorderColor = "green"
colorFg = "#fafafa"
colorBg = "#262729"

-- Generic settings
terminal' = "xfce4-terminal"
borderWidth' = 1
workspaces' =
    [ "one"
    , "two"
    , "three"
    , "four"
    , "five"
    , "six"
    , "seven"
    , "eight"
    , "nine"
    ]
windowSpacing = 5

-- Layouts
layouts = golden ||| half
    where goldenRatio       = (1+(toRational(sqrt(5)::Double)))/2
          renameLayout name = renamed [Replace name]
          golden            = renameLayout "G" $ Tall 1 (1/100) (1/goldenRatio)
          half              = renameLayout "H" $ Tall 1 (1/100) (1/2)

-- Explicit window management hooks
manageHooks = composeAll
    [ -- Center-floated windows
      className =? "Gmrun" --> doCenterFloat
    , className =? "ioquake3" --> doCenterFloat
    , className =? "Volumeicon" --> doCenterFloat
    , className =? "Settings" --> doCenterFloat
    , isFullscreen --> doFullFloat
    ]

-- Pretty-printer for xmobar
prettyPrinter handle = defaultPP
    -- FIXME: This seems to mess up the workspace click action.
    { ppCurrent = wrap (hl "[") (hl "]") . addWSAction
    , ppVisible = addWSAction
    , ppTitle = shorten 50
    -- FIXME: Remove the "Spacing 5" string added when using the smartSpacing
    --        layout modifier.
    , ppLayout = id
    , ppOutput = hPutStrLn handle
    }
    where addWSAction workspaceId =
            case (workspaceId `elemIndex` workspaces') of
                Just idx -> let idxString = show (idx+1)
                            in wrapAction idxString $ workspaceId
                Nothing  -> workspaceId
          wrapAction idx =
              wrap ("<action=`xdotool key alt+" ++ idx ++ "`>") "</action>"
          hl = xmobarColor colorHighlight ""

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
    , ((smMask, xK_h), prevWS)
    , ((smMask, xK_l), nextWS)
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
    , focusedBorderColor = focusBorderColor
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

