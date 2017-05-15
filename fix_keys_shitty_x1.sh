xcape -e 'Shift_R=Shift_R|parenright'
xcape -e 'Shift_L=Shift_L|parenleft'
setxkbmap -option ctrl:nocaps
xcape -e 'Control_L=Escape'
xmodmap -e "keycode 110 = Escape"
xmodmap -e "keycode 115 = Escape"
