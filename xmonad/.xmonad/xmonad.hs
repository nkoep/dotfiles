{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Control.Arrow (second)
import Data.List (intercalate, elemIndex)
import qualified Graphics.X11.Xlib (Rectangle(..))
import Graphics.X11.ExtraTypes.XF86
    ( xF86XK_MonBrightnessUp
    , xF86XK_MonBrightnessDown
    )
import System.IO (hPutStrLn)
import Text.Printf (printf)

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowBringer (gotoMenuArgs)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import qualified XMonad.Hooks.EwmhDesktops as E
import XMonad.Layout.Fullscreen (fullscreenManageHook)
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Gaps (gaps)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Renamed (renamed, Rename (Replace))
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Font (fi)
import XMonad.Util.Run (safeSpawn, spawnPipe)

-- Color definitions
colorHighlight = "#a51f1c"
focusBorderColor = "green"
colorFg = "#fafafa"
colorBg = "#262729"

-- Miscellaneous settings
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

-- Layout modifier `spacing`, copied from XMonad.Layout.Spacing so we can
-- override the implementation for `modifierDescription`.
data Spacing a = Spacing Int deriving (Show, Read)
instance LayoutModifier Spacing a where
    pureModifier (Spacing p) _ _ wrs =
        (map (second $ shrinkRect p) wrs, Nothing)
    modifierDescription (Spacing p) = ""

shrinkRect p (Rectangle x y w h) =
    Rectangle (x+fi p) (y+fi p) (w-2*fi p) (h-2*fi p)
spacing p = ModifiedLayout (Spacing p)

-- Layouts
layouts = golden ||| half ||| full
    where gr                = (1 + (toRational(sqrt(5) :: Double))) / 2
          renameLayout name = renamed [Replace name]
          golden            = renameLayout "G" $ Tall 1 (1 / 100) (1 / gr)
          half              = renameLayout "H" $ Tall 1 (1 / 100) (1 / 2)
          full              = renameLayout "F" $ Full

-- Explicit window management hooks
manageHooks = composeAll
    [ -- Center-floated windows
      className =? "Gmrun" --> doCenterFloat
    , className =? "ioquake3" --> doCenterFloat
    , className =? "Volumeicon" --> doCenterFloat
    , className =? "Settings" --> doCenterFloat
    , className =? "Gcr-prompter" --> doCenterFloat
    , isFullscreen --> doFullFloat
    ]

-- Pretty-printer for xmobar
workspaceIndex workspaceId =
    case (workspaceId `elemIndex` workspaces') of
        Just idx -> (idx+1)
        Nothing  -> 0 -- unreachable

wsAction idx =
    wrap ("<action=`xdotool key alt+" ++ show idx ++ "`>") "</action>"

formatWSName l r name =
    let idx = workspaceIndex name
    in wsAction idx $ wrap l r name

prettyPrinter handle = defaultPP
    { ppCurrent = formatWSName (hl "[") (hl "]")
    , ppHidden = formatWSName (bg "[") (bg "]")
    , ppSep = hl " - "
    , ppTitle = shorten 75
    , ppOutput = hPutStrLn handle
    }
    where hl = xmobarColor colorHighlight ""
          bg = xmobarColor colorBg ""

-- dmenu customizations (requires the `dmenu-xft-height` AUR package)
dmenuOptions =
    [ "-b"
    , "-i"
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
userScript = spawn . (++) "~/Dropbox/bla/.bin/"

keybindings =
    [ ((alt, xK_p), safeSpawn "dmenu_run" dmenuOptions)
    , ((alt, xK_n), spawn "nautilus")
    , ((shiftAlt, xK_f), gotoMenuArgs dmenuOptions)
    , ((alt, xK_F5), spawn "slock")
    , ((alt, xK_F6), userScript "audio-toggle")
    , ((shiftAlt, xK_h), prevWS)
    , ((shiftAlt, xK_l), nextWS)
    , ((shiftControlAlt, xK_h), shiftToPrev)
    , ((shiftControlAlt, xK_l), shiftToNext)
    , ((alt, xK_Up), raiseBrightness)
    , ((alt, xK_Down), lowerBrightness)
    ]
    where alt                = mod1Mask
          shiftAlt           = shiftMask .|. alt
          controlAlt         = controlMask .|. alt
          shiftControlAlt    = shiftMask .|. controlAlt
          brightnessStep     = "5%"
          adjustBrightness o = safeSpawn "xbacklight" [o : brightnessStep]
          raiseBrightness    = adjustBrightness '+'
          lowerBrightness    = adjustBrightness '-'

startupHook' = do
    spawn "autorandr -c"
    setDefaultCursor xC_left_ptr
    spawn "xbacklight -set 80%"
    userScript "init-desktop"

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
