import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

import XMonad
import XMonad.Actions.CycleWS (nextWS, prevWS)
import XMonad.Actions.WindowBringer (gotoMenuArgs)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Hooks.ManageHelpers (isDialog, doCenterFloat)
import XMonad.Layout.Gaps (gaps, Direction2D(U, D, L, R))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Renamed (renamed, Rename (Replace))
import XMonad.Layout.Spacing (spacing)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (removeKeys)
import XMonad.Util.Run (safeSpawn)

-- Layouts
layouts = tiled ||| half ||| full
    where goldenRatio = (1 + (toRational(sqrt(5) :: Double))) / 2
          tiled       = Tall 1 (1 / 100) (1 / goldenRatio)
          half        = Tall 1 (1 / 2) (1 / 2)
          full        = Full

windowSpacing :: Int
windowSpacing = 5

config' borderColor focusColor = (ewmhFullscreen . ewmh . docks) def
    { normalBorderColor = fromMaybe "white" borderColor
    , focusedBorderColor = fromMaybe "yellow" focusColor
    , terminal = "kitty"
    , layoutHook
        = avoidStruts
        $ gaps (zip [U, D, L, R] $ repeat windowSpacing)
        $ spacing windowSpacing
        $ smartBorders
        $ layouts
    , startupHook = do setDefaultCursor xC_left_ptr
    , manageHook = isDialog --> doCenterFloat
    , borderWidth = 2
    , focusFollowsMouse = False
    , clickJustFocuses = False
    }
    `removeKeys` [ (mod1Mask, xK_q)  -- Restart xmonad
                 , (mod1Mask .|. shiftMask, xK_q)  -- Quit xmonad
                 , (mod1Mask .|. shiftMask, xK_p)  -- dmenu
                 ]

main :: IO ()
main = do
    borderColor <- lookupEnv "X_THEME_FG"
    focusColor <- lookupEnv "X_THEME_PRIMARY"
    safeSpawn "polybar-xmonad" []
    xmonad $ config' borderColor focusColor
