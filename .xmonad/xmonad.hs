import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

import qualified XMonad.Actions.CycleWS as CWS
import qualified XMonad.Layout.WindowNavigation as WN

-- Data.List provides isPrefixOf isSuffixOf and isInfixOf
import Data.List
myManageHook = composeAll . concat $
   [ [(className =? "Firefox" <&&> resource =? "Dialog") --> doFloat]
   , [ className =? "Gimp" --> doFloat ]
   , [ className =? "Evince" --> doFloat ]
   , [ fmap ( c `isInfixOf`) title     --> doFloat | c <- myMatchAnywhereFloatsT ]
   ]
  where myMatchAnywhereFloatsT = ["vistrails", "VisTrails"]


main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/mates/.xmobarrc"
    xmonad $ defaultConfig {
        manageHook = myManageHook <+> manageHook defaultConfig,
        layoutHook = avoidStruts  $  layoutHook defaultConfig,
        logHook = dynamicLogWithPP $ xmobarPP
                    { ppOutput = hPutStrLn xmproc,
                      ppTitle = xmobarColor "green" "" . shorten 50
                    },
        modMask = mod4Mask,     -- Rebind Mod to the Windows key 
        terminal = "sakura"
        } `additionalKeys`
        [ ((0, xK_Print), spawn "scrot"),
        ((mod4Mask .|. shiftMask, xK_l), spawn "i3lock"),
        ((mod4Mask, xK_s), spawn "firefox"),
        ((mod4Mask, xK_d), spawn "thunar"),
        ((mod4Mask, xK_Right), CWS.nextScreen),
        ((mod4Mask, xK_Left), CWS.prevScreen),
        ((mod4Mask .|. controlMask, xK_Right), sendMessage $ WN.Go WN.R),
        ((mod4Mask .|. controlMask, xK_Left ), sendMessage $ WN.Go WN.L)
        ]
