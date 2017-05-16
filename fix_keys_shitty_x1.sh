# Fix the horrible keyboard layout of the 2nd gen Thinkpad Carbon X1.
#
# This will the Home and End buttons to Control when held and Escape
# when tapped. Left shift and right shift each will generate
# parenthesis when tapped. This mapping destroys the Home and End
# keycodes, but I never use them for anything anyway.

xcape -e 'Shift_R=Shift_R|parenright'
xcape -e 'Shift_L=Shift_L|parenleft'
setxkbmap -option ctrl:nocaps
xcape -e 'Control_L=Escape'

spare_modifier="Hyper_L"
xmodmap -e "keycode 110 = $spare_modifier"
xmodmap -e "keycode 115 = $spare_modifier"
xmodmap -e "remove mod4 = $spare_modifier" # hyper_l is mod4 by default
xmodmap -e "add Control = $spare_modifier"
xcape -e 'Hyper_L=Escape'
