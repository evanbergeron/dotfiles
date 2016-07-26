import XMonad
-- import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig

main = xmonad $ defaultConfig {
    borderWidth = 2
  , terminal = "urxvt"
  , normalBorderColor = "#073642"
  , focusedBorderColor = "#586e75"
  }

