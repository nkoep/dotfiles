{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Control.Arrow (second)
import Control.Monad (when, join)
import System.Directory (doesFileExist, removeFile)
import Data.List (elemIndex)
import Data.Maybe (maybeToList)
import qualified Graphics.X11.Xlib (Rectangle(..))
import Codec.Binary.UTF8.String (decodeString)

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowBringer (gotoMenuArgs)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops as E
import XMonad.Layout.Fullscreen as LF
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Gaps (gaps)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Renamed (renamed, Rename (Replace))
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (additionalKeys, removeKeys)
import XMonad.Util.Font (fi)
import XMonad.Util.Run (safeSpawn, safeSpawnProg)

-- Add support for fullscreen videos in Firefox.
addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

-- Color definitions
colorHighlight = "#a51f1c"
focusBorderColor = "green"
colorFg = "#fafafa"
colorBg = "#262729"

-- Miscellaneous settings
terminal' = "xfce4-terminal"
borderWidth' = 2
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
layouts = tiled ||| half ||| full
    where gr                = (1 + (toRational(sqrt(5) :: Double))) / 2
          renameLayout name = renamed [Replace name]
          tiled             = renameLayout "T" $ Tall 1 (1 / 100) (1 / gr)
          half              = renameLayout "H" $ Tall 1 (1 / 2) (1 / 2)
          full              = renameLayout "F" $ Full

-- Float dialog windows
floatHooks = isDialog --> doCenterFloat

-- Pretty-printer for xmobar
workspaceIndex workspaceId =
    case (workspaceId `elemIndex` workspaces') of
        Just idx -> (idx+1)
        Nothing  -> 0 -- unreachable

prettyPrinter file = defaultPP
    { ppCurrent = colorBracket hl
    , ppHidden = colorBracket bg
    , ppSep = hl " - "
    , ppTitle = shorten 60
    , ppOutput = \s -> appendFile file . decodeString $ (s ++ "\n")
    }
    where polybarColor c = wrap ("%{F" ++ c ++ "}") "%{F-}"
          hl = polybarColor colorHighlight
          bg = polybarColor colorBg
          colorBracket c = wrap (c "[") (c "]")

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
keybindings =
    [ ((alt, xK_p), safeSpawn "dmenu_run" dmenuOptions)
    , ((alt, xK_n), safeSpawnProg "nautilus")
    , ((alt, xK_f), gotoMenuArgs dmenuOptions)
    , ((alt, xK_F5), safeSpawnProg "slock")
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
    setDefaultCursor xC_left_ptr
    safeSpawn "xbacklight" ["-set", "50%"]
    spawn "feh --bg-fill ~/Dropbox/bla/wallpaper"

config' logfile = E.ewmh defaultConfig
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
        <+> floatHooks
        <+> LF.fullscreenManageHook
    , handleEventHook
        =   docksEventHook
        <+> E.fullscreenEventHook
    , workspaces = workspaces'
    , borderWidth = borderWidth'
    , logHook = dynamicLogWithPP $ prettyPrinter logfile
    , startupHook = startupHook' >> addEWMHFullscreen
    , focusFollowsMouse = False
    , clickJustFocuses = False
    } `additionalKeys` keybindings `removeKeys` restartCombo
    where restartCombo = [(mod1Mask, xK_q)]

-- xmobar configuration
main :: IO ()
main = do
    let pipe = "/tmp/.xmonad.log"
    fileExists <- doesFileExist pipe
    when fileExists $ removeFile pipe
    safeSpawn "mkfifo" [pipe]
    safeSpawn "polybar" ["bla"]
    xmonad $ config' pipe
