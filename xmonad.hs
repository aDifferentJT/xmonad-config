import XMonad
import XMonad.StackSet (focusDown, shift, shiftWin, stack, tag, integrate, currentTag)
import qualified XMonad.StackSet
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.CopyWindow
import XMonad.Util.Run(runProcessWithInput, spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.NamedWindows
import XMonad.Util.Dmenu
import XMonad.Util.SpawnOnce
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import System.IO
import System.Process
import Data.List
import Data.List.Split
import Data.Maybe
import Control.Monad
import Graphics.X11.ExtraTypes.XF86

-- Modify dmenu to include -i (case insensitive) option
dmenuLaunch :: X ()
dmenuLaunch = spawn =<< myDmenu =<< splitOn "\n" <$> runProcessWithInput "bash" ["-c", "compgen -c"] ""

myDmenu :: [String] -> X String
myDmenu = menuArgs "dmenu" ["-i", "-nf", "green", "-nb", "black", "-sf", "black", "-sb", "green"]

myManageHook = composeAll [
                 isDialog --> doCenterFloat,
                 isFullscreen --> (doF focusDown <+> doFullFloat)
                 --className =? "GNUMail" --> doFloat
                 ]

myStartupHook = do
  spawnOnce "trayer --edge top --align right --margin 0 --distance 0 --distancefrom right --widthtype percent --width 10 --heighttype pixel --height 15 --SetDockType true --SetPartialStrut true --transparent true --tint 0x000000 --alpha 255 --expand true --padding 0 --monitor primary"
  spawnOnce "nm-applet"
  spawnOnce "caffeine-indicator"
  spawnOnce "redshift-gtk"
  spawnOnce "blueman-applet"
  spawnOnce "bash -c trayFaders"
  spawnOnce "xsetroot -solid black"

myKeys = [
  ((controlMask .|. mod1Mask, xK_v), spawn "/bin/bash -c printclip"),
  ((myModMask, xK_p), dmenuLaunch),
  ((myModMask, xK_b), sendMessage ToggleStruts),
  ((myModMask, xK_v), windows copyToAll),
  ((myModMask .|. shiftMask, xK_v), killAllOtherCopies),
  ((myModMask, xK_Escape), windows $ shift minimiseWorkspace),
  ((myModMask, xK_grave), withWindowSet
    (\winSet ->
      maybe (return ())
        (\wins -> 
          (\win -> windows $ shiftWin (currentTag winSet) win)
          =<< head
          <$> (
            (\title -> filterM (((== title) <$> show <$>) . getName) (integrate wins))
            =<< myDmenu
            =<< (
              sequence
              . map ((show <$>) . getName)
              . integrate
              $ wins
              )
            )
          )
      . stack
      . head
      . filter ((== minimiseWorkspace) . tag)
      . XMonad.StackSet.workspaces
      $ winSet
      )
    ),
  ((myModMask, xF86XK_MonBrightnessDown), spawn "bash -c \"setBrightness 1\""),
  ((myModMask .|. shiftMask, xF86XK_MonBrightnessDown), spawn "bash -c \"setBrightness 0\""),
  ((myModMask, xF86XK_MonBrightnessUp), spawn "bash -c \"setBrightness $(cat /sys/class/backlight/intel_backlight/max_brightness)\"")
  ]

minimiseWorkspace = "Min"
myWorkspaces = map show [1..9] ++ [minimiseWorkspace]

myModMask = mod4Mask

main = do
  xmproc <- spawnPipe "xmobar"
  (xmonad . ewmh . flip additionalKeys myKeys) def {
    manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig,
    layoutHook = smartBorders $ avoidStruts  $  layoutHook defaultConfig,
    handleEventHook = handleEventHook def <+> fullscreenEventHook,
    startupHook = myStartupHook,
    workspaces = myWorkspaces,
    terminal = "xterm -fg green -bg black",
    modMask = myModMask,
    logHook = dynamicLogWithPP $ xmobarPP {
                ppOutput = hPutStrLn xmproc,
                ppTitle = xmobarColor "green" "" . shorten 50,
                ppCurrent = xmobarColor "black" "green",
                ppHidden  = xmobarColor "green" "black"
                }
    }
    
